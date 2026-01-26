#' @title carbonLTS
#' @description reads carbon stored in harvested timber out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any secdforest aggregation level defined in superAggregateX
#' @param unit element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr;
#' "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr
#' @param cumulative Logical; Determines if cHWP emissions are reported annually (FALSE)
#' or cumulative (TRUE). The starting point for cumulative emissions is y1995.
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @return carbon stocks in MtC from harvested timber
#' @details Annual (and cumulative) Carbon stored in harvested wood products,
#' as well as, slow emissions from half life deacy.
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
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
  if (as.numeric(readGDX(gdx, "s73_timber_demand_switch", "sm_timber_demand_switch", format = "first_found")) == 1) {
    timber <- TRUE
  }

  if (timber) { # read wood and woodfuel from a model run
    kforestry <- readGDX(gdx, "kforestry")

    # Production
    # mio. tDM -- Includes construction wood (if high demand scenarios)
    inflow <- collapseNames(readGDX(gdx, "ov_prod")[, , kforestry][, , "level"])
    # Create additional category as this constr_wood needs to be separated from wood
    inflow <- add_columns(x = inflow, addnm = "constr_wood")
    inflow[, , "constr_wood"] <- 0

    # Overall timber -- Same as production but used much later in code
    overallWoodRemoval <- inflow # MtDM

    ## Read volume information
    volume <- readGDX(gdx, "f73_volumetric_conversion")
    if (is.null(volume)) {
      volume <- 0.6
    } else {
      volume <- add_columns(x = volume, addnm = "constr_wood")
      volume[, , "constr_wood"] <- volume[, , "wood"]
    }
    # See how much constr_wood was demanded. Even in BAU case there is a teeny tiny amount of wood for building construction
    # This is regional, we will distribute it to cells based on a simple weight
    p73DemandConstrWood <- readGDX(gdx, "p73DemandConstrWood", react = "silent")
    if (!is.null(p73DemandConstrWood)) {
      inflow[, , "constr_wood"] <- p73DemandConstrWood[, getYears(inflow), ] * (inflow[, , "wood"] + 10^(-10)) / superAggregateX(data = (inflow[, , "wood"] + 10^(-10)), aggr_type = "sum", level = "reg")
      inflow[, , "wood"] <- inflow[, , "wood"] - inflow[, , "constr_wood"] # Wood already contained building demand which we now remove
      inflow[inflow < 0] <- 0 # Regions where all constr_wood demand is more than what was overall produced
      overallWoodRemoval <- inflow # Create (recreate) overallWoodRemoval
    }
    # Convert to Volume -- mio.tDM to mio.m3
    inflow <- inflow / volume

    # Conversion factor 230kgC/m3 -- Table S2 in SI of https://doi.org/10.1088/1748-9326/7/3/034023
    # Unit below = million KgC
    inflow <- inflow * 230 # bit lower than mean value
    inflow <- inflow / 1e3 # Conversion to million ton C

    # Find out which years have to be extrapolated - DONT GO TOO BACK IN TIME TO AVOID NEGATIVE VALUES from time_interpolate
    allYears <- paste0("y", 1970:2150)

    # Find misssing year
    missing <- setdiff(allYears, getYears(inflow))

    # Interpolate for missing years
    inflow <- time_interpolate(
      dataset = inflow,
      interpolated_year = missing,
      integrate_interpolated_years = TRUE,
      extrapolation_type = "linear"
    )
    inflow[inflow < 0] <- 0


    # Regional end use demand
    prodSpecific <- dimSums(readGDX(gdx, "p73_forestry_demand_prod_specific"), dim = 1)

    # Extract pulpwood demand
    # See Figure 5 in DOI 10.1186/s13021-015-0016-7
    prodSpecific[, , "pulpwood"] <- prodSpecific[, , "industrial_roundwood"] - (prodSpecific[, , "sawlogs_and_veneer_logs"] + prodSpecific[, , "other_industrial_roundwood"])

    # Interpolate
    prodSpecific <- time_interpolate(
      dataset = prodSpecific,
      interpolated_year = missing,
      integrate_interpolated_years = TRUE,
      extrapolation_type = "linear"
    )

    # Isolate produced stuff
    endUseShare <- mbind(prodSpecific)[, , c("sawlogs_and_veneer_logs", "other_industrial_roundwood", "pulpwood")] # Not really great to call endUseShare here as the shares are actually calculated below

    # Calculate share for each wood category
    endUseShare <- endUseShare / dimSums(endUseShare, dim = c(3))

    # Disaggregate inflow into categories
    inflowDisagg <- collapseNames(dimSums(inflow[, , c("wood")], dim = 3)) * endUseShare[, getYears(inflow), ] # mio. tC

    # Add back constr_wood in disaggregated data
    inflowDisagg <- mbind(inflowDisagg, inflow[, , "constr_wood"])


    # IPCC method to calculate HWO stock
    # See Equation 4 in DOI 10.1186/s13021-015-0016-7
    # See Equation in section 2.1 page 2 in DOI http://dx.doi.org/10.1088/1748-9326/7/3/034023
    # See Equation 12.2 in "2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories"
    # "Chapter 12: Harvested Wood Products"
    # https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch12_HarvestedWoodProducts.pdf

    # Some Definitions
    # k= decay constant of FOD for each HWP category (HWPj)
    # given in units yr-1(k= ln(2)/HL, where HL is half-life of the HWP pool in years (see Section 2.8.3.2).

    # Create half life data frame
    halfLifeDf <- data.frame(
      var = c("sawlogs_and_veneer_logs", "other_industrial_roundwood", "pulpwood", "constr_wood"),
      half_life = c(30, 20, 2, 60)
    )

    k <- round(log(2) / (as.magpie(halfLifeDf)), 8) # See http://dx.doi.org/10.1088/1748-9326/7/3/034023 or IPCC doc linked above

    alpha <- exp(-k) # See http://dx.doi.org/10.1088/1748-9326/7/3/034023 or IPCC doc linked above

    beta <- ((1 - alpha) / k) # See http://dx.doi.org/10.1088/1748-9326/7/3/034023 or IPCC doc linked above

    # Create empty object
    stock <- inflowDisagg
    stock[stock > 0] <- 0

    # Create old Stock
    # See Excel file linked in SI of http://dx.doi.org/10.1088/1748-9326/7/3/034023
    previousStock <- new.magpie(cells_and_regions = "GLO", years = c("y1970"), fill = c(4271))
    previousStock <- previousStock * (setYears(inflowDisagg[, "y1970", ] / dimSums(inflowDisagg[, "y1970", ], dim = c(1, 3)), NULL))
    previousStock <- collapseNames(setYears(previousStock, NULL))

    # Disaggregate first stock - as we do not know historical stock in buildings we can set it to 0 when spooling from 1970
    endUseShare <- add_columns(x = endUseShare, addnm = "constr_wood", dim = 3.1)
    endUseShare[, , "constr_wood"] <- 0
    stock[, "y1970", ] <- dimSums(previousStock, dim = 3) * endUseShare[, "y1970", ]

    # From this stock, some outflow will happen, due to decay

    # Create inflow to be added
    inflowToAdd <- inflowDisagg
    inflowToAdd[inflowToAdd != 0] <- 0

    # Calculate inflow
    inflowToAdd <- beta * inflowDisagg

    # See http://dx.doi.org/10.1088/1748-9326/7/3/034023 for description of this algorithm
    for (i in 1:(length(getYears(stock)) - 1)) {
      # Update stock for next step
      stock[, i + 1, ] <- alpha * stock[, i, ] + inflowToAdd[, i, ]
    }

    # Calculate outflow
    outflow <- stock - alpha * stock

    # Convert to Volume -- mio.tDM to mio.m3
    # VERY IMPORTANT that overallWoodRemoval has no volume related transformations before
    overallWoodRemoval <- overallWoodRemoval / volume

    # Conversion factor 230kgC/m3 -- Table S2 in SI of https://doi.org/10.1088/1748-9326/7/3/034023
    # Unit below = million KgC
    overallWoodRemoval <- overallWoodRemoval * 230 # bit lower than mean value
    overallWoodRemoval <- overallWoodRemoval / 1e3 # Conversion to million ton C

    # Interpolate
    overallWoodRemoval <- time_interpolate(
      dataset = overallWoodRemoval,
      interpolated_year = missing,
      integrate_interpolated_years = TRUE,
      extrapolation_type = "linear"
    )

    # Bind together
    a <- mbind(
      setNames(-1 * dimSums(stock, dim = 3), "anthropogenic_stock"),
      setNames(-1 * dimSums(stock[, , "constr_wood", invert = TRUE], dim = 3), "wood_stocks"),
      setNames(-1 * dimSums(stock[, , "constr_wood"], dim = 3), "building_stocks"),
      setNames(-1 * dimSums(inflowToAdd, dim = 3), "annual_inflow"),
      setNames(-1 * dimSums(inflowToAdd[, , "constr_wood", invert = TRUE], dim = 3), "wood_inflow"),
      setNames(-1 * dimSums(inflowToAdd[, , "constr_wood"], dim = 3), "building_inflow"),
      setNames(dimSums(-1 * inflowToAdd + outflow, dim = 3), "net_sink_HWP"),
      setNames(dimSums(-1 * inflowToAdd[, , "constr_wood", invert = TRUE] + outflow[, , "constr_wood", invert = TRUE], dim = 3), "net_sink_wood"),
      setNames(dimSums(-1 * inflowToAdd[, , "constr_wood"] + outflow[, , "constr_wood"], dim = 3), "net_sink_building"),
      setNames(dimSums(outflow, dim = 3), "annual_outflow"),
      setNames(dimSums(outflow[, , "constr_wood", invert = TRUE], dim = 3), "wood_outflow"),
      setNames(dimSums(outflow[, , "constr_wood"], dim = 3), "building_outflow"),
      setNames(overallWoodRemoval[, , "wood"], "emis_wood"),
      setNames(overallWoodRemoval[, , "woodfuel"], "emis_woodfuel"),
      setNames(overallWoodRemoval[, , "constr_wood"], "emis_constr_wood")
    )

    if (cumulative) {
      years <- getYears(a, as.integer = TRUE)
      imYears <- new.magpie("GLO", years, NULL)
      imYears[, , ] <- c(1, diff(years))
      a[, "y1995", ] <- 0
      a <- a * imYears[, getYears(a), ]
      a <- as.magpie(apply(a, c(1, 3), cumsum))
      a <- a - setYears(a[, baseyear, ], NULL)
    }

    if (level != "cell") {
      a <- superAggregateX(a, aggr_type = "sum", level = level)
    }

    ## Conversion based on unit
    if (unit == "gas") {
      a <- a * 44 / 12
    }
  } else {
    a <- NULL
    message("Disabled (no timber) ", appendLF = FALSE)
  }

  out(a, file)
}
