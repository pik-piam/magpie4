#' @title carbonLTS
#' @description reads carbon stored in harvested timber out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param unit element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr
#' @param cumulative Logical; Determines if cHWP emissions are reported annually (FALSE) or cumulative (TRUE). The starting point for cumulative emissions is y1995.
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @return carbon stocks in MtC from harvested timber
#' @details Annual (and cumulative) Carbon stored in harvested wood products as well as slow emissions from half life deacy.
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @importFrom utils tail
#' @examples
#' \dontrun{
#' x <- carbonLTS(gdx)
#' }
#'
carbonLTS <- function(gdx,
                      file = NULL,
                      level = "cell",
                      unit = "element",
                      cumulative = FALSE,
                      baseyear = 1995) {
  timber <- FALSE
  if (as.numeric(readGDX(gdx, "s73_timber_demand_switch", "sm_timber_demand_switch")) == 1) timber <- TRUE

  if (timber) { ## read wood and woodfuel from a model run
    kforestry <- readGDX(gdx, "kforestry")

    ## Production
    inflow <- collapseNames(readGDX(gdx, "ov_prod")[, , kforestry][, , "level"]) ## mio. tDM -- Includes construction wood (if high demand scenarios)
    inflow <- add_columns(x = inflow, addnm = "constr_wood") ## Create additional category as this constr_wood needs to be separated from wood
    inflow[, , "constr_wood"] <- 0

    ## Overall timber -- Same as production but used much later in code
    overall_wood_removal <- inflow ## MtDM

    ### Read volume information
    volume <- readGDX(gdx, "f73_volumetric_conversion")
    if (is.null(volume)) {
      volume <- 0.6
    } else {
      volume <- add_columns(x = volume, addnm = "constr_wood")
      volume[, , "constr_wood"] <- volume[, , "wood"]
    }
    ## See how much constr_wood was demanded. Even in BAU case there is a teeny tiny amount of wood for building construction
    ## This is regional, we will distribute it to cells based on a simple weight
    p73_demand_constr_wood <- readGDX(gdx, "p73_demand_constr_wood", react = "silent")
    if (!is.null(p73_demand_constr_wood)) {
      inflow[, , "constr_wood"] <- p73_demand_constr_wood[, getYears(inflow), ] * (inflow[, , "wood"] + 10^(-10)) / superAggregate(data = (inflow[, , "wood"] + 10^(-10)), aggr_type = "sum", level = "reg")
      inflow[, , "wood"] <- inflow[, , "wood"] - inflow[, , "constr_wood"] ## Wood already contained building demand which we now remove
      inflow[inflow < 0] <- 0 ## Regions where all constr_wood demand is more than what was overall produced
      overall_wood_removal <- inflow ## Create (recreate) overall_wood_removal
    }
    ## Convert to Volume -- mio.tDM to mio.m3
    inflow <- inflow / volume

    ## Conversion factor 230kgC/m3 -- Table S2 in SI of https://doi.org/10.1088/1748-9326/7/3/034023
    ## Unit below = million KgC
    inflow <- inflow * 230 ## bit lower than mean value
    inflow <- inflow / 1e3 ## Conversion to million ton C

    ## Find out which years have to be extrapolated - DONT GO TOO BACK IN TIME TO AVOID NEGATIVE VALUES from time_interpolate
    all_years <- paste0("y", 1970:2150)

    ## Find misssing year
    missing <- setdiff(all_years, getYears(inflow))

    ## Interpolate for missing years
    inflow <- time_interpolate(
      dataset = inflow,
      interpolated_year = missing,
      integrate_interpolated_years = T,
      extrapolation_type = "linear"
    )
    inflow[inflow < 0] <- 0


    ## Regional end use demand
    prod_specific <- dimSums(readGDX(gdx, "p73_forestry_demand_prod_specific"), dim = 1)

    ## Extract pulpwood demand
    ## See Figure 5 in DOI 10.1186/s13021-015-0016-7
    prod_specific[, , "pulpwood"] <- prod_specific[, , "industrial_roundwood"] - (prod_specific[, , "sawlogs_and_veneer_logs"] + prod_specific[, , "other_industrial_roundwood"])

    ## Interpolate
    prod_specific <- time_interpolate(
      dataset = prod_specific,
      interpolated_year = missing,
      integrate_interpolated_years = T,
      extrapolation_type = "linear"
    )

    ## Isolate produced stuff
    end_use_share <- mbind(prod_specific)[, , c("sawlogs_and_veneer_logs", "other_industrial_roundwood", "pulpwood")] ## Not really great to call end_use_share here as the shares are actually calculated below

    ## Calculate share for each wood category
    end_use_share <- end_use_share / dimSums(end_use_share, dim = c(3))

    ## Disaggregate inflow into categories
    inflow_disagg <- collapseNames(dimSums(inflow[, , c("wood")], dim = 3)) * end_use_share[, getYears(inflow), ] ## mio. tC

    ## Add back constr_wood in disaggregated data
    inflow_disagg <- mbind(inflow_disagg, inflow[, , "constr_wood"])
    # dimSums(inflow_disagg, dim = c(1, 3))
    # dimSums(inflow_disagg, dim = c(1))
    # dimSums(inflow[, , "wood"], dim = 1)


    ## IPCC method to calculate HWO stock
    ## See Equation 4 in DOI 10.1186/s13021-015-0016-7
    ## See Equation in section 2.1 page 2 in DOI http://dx.doi.org/10.1088/1748-9326/7/3/034023
    ## See Equation 12.2 in "2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories"
    ## "Chapter 12: Harvested Wood Products"
    ## https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch12_HarvestedWoodProducts.pdf

    ## Some Definitions
    ## k= decay constant of FOD for each HWP category (HWPj)
    ## given in units yr-1(k= ln(2)/HL, where HL is half-life of the HWP pool in years (see Section 2.8.3.2).

    ## Create half life data frame
    half_life_df <- data.frame(
      var = c("sawlogs_and_veneer_logs", "other_industrial_roundwood", "pulpwood", "constr_wood"),
      half_life = c(30, 20, 2, 60)
    )

    k <- round(log(2) / (as.magpie(half_life_df)), 8) ## See http://dx.doi.org/10.1088/1748-9326/7/3/034023 or IPCC doc linked above

    alpha <- exp(-k) ## See http://dx.doi.org/10.1088/1748-9326/7/3/034023 or IPCC doc linked above

    beta <- ((1 - alpha) / k) ## See http://dx.doi.org/10.1088/1748-9326/7/3/034023 or IPCC doc linked above

    ## Create empty object
    stock <- inflow_disagg
    stock[stock > 0] <- 0

    ## Create old Stock
    ## See Excel file linked in SI of http://dx.doi.org/10.1088/1748-9326/7/3/034023
    previous_stock <- new.magpie(cells_and_regions = "GLO", years = c("y1970"), fill = c(4271)) # in MtC SI https://doi.org/10.1088/1748-9326/7/3/034023
    previous_stock <- collapseNames(setYears(previous_stock
    * (setYears(inflow_disagg[, "y1970", ]
      / dimSums(inflow_disagg[, "y1970", ],
          dim = c(1, 3)
        ), NULL)), NULL))
    # dimSums(previous_stock, dim = 1)

    ## Create first inflow
    first_inflow <- new.magpie(cells_and_regions = "GLO", years = c("y1971"), fill = c(220)) # in MtC SI https://doi.org/10.1088/1748-9326/7/3/034023
    first_inflow <- collapseNames(setYears(first_inflow * (setYears(inflow_disagg[, "y1995", ] / dimSums(inflow_disagg[, "y1995", ], dim = c(1, 3)), NULL)), NULL))
    # dimSums(first_inflow, dim = 1)

    ## Disaggregate first stock - as we do not know historical stock in buildings we can set it to 0 when spooling from 1970
    end_use_share <- add_columns(x = end_use_share, addnm = "constr_wood", dim = 3.1)
    end_use_share[, , "constr_wood"] <- 0
    stock[, "y1970", ] <- dimSums(previous_stock, dim = 3) * end_use_share[, "y1970", ]
    # dimSums(stock, dim = c(1, 3))[, 1994:2000]

    ## From this stock, some outflow will happen, due to decay

    ## Create outlow
    outflow <- inflow_disagg
    outflow[outflow != 0] <- 0
    # dimSums(outflow, dim = c(1, 3))

    ## Create annual inflow
    annual_inflow <- outflow

    ## Create stock staying stock
    stock_in_place <- stock

    ## Create inflow to be added
    inflow_to_add <- inflow_disagg
    inflow_to_add[inflow_to_add != 0] <- 0

    for (i in 1:(length(getYears(stock)) - 1)) { ## See http://dx.doi.org/10.1088/1748-9326/7/3/034023 for description of this algorithm

      ## Stock in place
      stock_in_place[, i, ] <- alpha * stock[, i, ]

      ## How much new inflow comes
      inflow_to_add[, i, ] <- beta * inflow_disagg[, i, ]

      # Calculate outflow
      outflow[, i, ] <- stock[, i, ] - stock_in_place[, i, ]

      # Update stock for next step
      stock[, i + 1, ] <- stock_in_place[, i, ] + inflow_to_add[, i, ]
    }

    ## Convert to Volume -- mio.tDM to mio.m3
    overall_wood_removal <- overall_wood_removal / volume ### VERY IMPORTANT that overall_wood_removal has no volume related transformations before

    ## Conversion factor 230kgC/m3 -- Table S2 in SI of https://doi.org/10.1088/1748-9326/7/3/034023
    ## Unit below = million KgC
    overall_wood_removal <- overall_wood_removal * 230 ## bit lower than mean value
    overall_wood_removal <- overall_wood_removal / 1e3 ## Conversion to million ton C

    ## Interpolate
    overall_wood_removal <- time_interpolate(
      dataset = overall_wood_removal,
      interpolated_year = missing,
      integrate_interpolated_years = T,
      extrapolation_type = "linear"
    )

    ## Bind together
    a <- mbind(
      setNames(-1 * dimSums(stock, dim = 3), "anthropogenic_stock"),
      setNames(-1 * dimSums(stock[, , "constr_wood", invert = TRUE], dim = 3), "wood_stocks"),
      setNames(-1 * dimSums(stock[, , "constr_wood"], dim = 3), "building_stocks"),
      setNames(-1 * dimSums(inflow_to_add, dim = 3), "annual_inflow"),
      setNames(-1 * dimSums(inflow_to_add[, , "constr_wood", invert = TRUE], dim = 3), "wood_inflow"),
      setNames(-1 * dimSums(inflow_to_add[, , "constr_wood"], dim = 3), "building_inflow"),
      setNames(dimSums(-1 * inflow_to_add + outflow, dim = 3), "net_sink_HWP"),
      setNames(dimSums(-1 * inflow_to_add[, , "constr_wood", invert = TRUE] + outflow[, , "constr_wood", invert = TRUE], dim = 3), "net_sink_wood"),
      setNames(dimSums(-1 * inflow_to_add[, , "constr_wood"] + outflow[, , "constr_wood"], dim = 3), "net_sink_building"),
      setNames(dimSums(outflow, dim = 3), "annual_outflow"),
      setNames(dimSums(outflow[, , "constr_wood", invert = TRUE], dim = 3), "wood_outflow"),
      setNames(dimSums(outflow[, , "constr_wood"], dim = 3), "building_outflow"),
      setNames(overall_wood_removal[, , "wood"], "emis_wood"),
      setNames(overall_wood_removal[, , "woodfuel"], "emis_woodfuel"),
      setNames(overall_wood_removal[, , "constr_wood"], "emis_constr_wood")
    )

    if (cumulative) {
      years <- getYears(a, as.integer = T)
      im_years <- new.magpie("GLO", years, NULL)
      im_years[, , ] <- c(1, diff(years))
      a[, "y1995", ] <- 0
      a <- a * im_years[, getYears(a), ]
      a <- as.magpie(apply(a, c(1, 3), cumsum))
      a <- a - setYears(a[, baseyear, ], NULL)
    }

    if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level, na.rm = FALSE)

    ### Conversion based on unit
    if (unit == "gas") a <- a * 44 / 12
  } else {
    a <- NULL
    message("Disabled (no timber) ", appendLF = FALSE)
  }

  out(a, file)
}
