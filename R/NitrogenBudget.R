#' @title NitrogenBudget
#' @description calculates projections of Nitrogen Budgets for Croplands (Tg Nr per) from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param include_emissions TRUE also divides the N surplus into different emissions
#' @param level aggregation level, reg, glo or regglo, cell, iso or grid
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @param debug debug mode TRUE makes some consistency checks between estimates for different resolutions.
#' @param cropTypes FALSE for aggregate results; TRUE for crop-specific results
#' @param threshold passed to mstools::toolFertilizerDistribution
#' @param progress passed to mstools::toolFertilizerDistribution
#' @author Benjamin Leon Bodirsky, Michael Crawford, Edna J. Molina Bacca, Florian Humpenoeder
#' @importFrom magpiesets findset
#' @importFrom madrat toolAggregate
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom luscale superAggregate
#' @importFrom mstools toolFertilizerDistribution
#' @examples
#' \dontrun{
#' x <- NitrogenBudget(gdx)
#' }
#'
NitrogenBudget <- function(gdx, include_emissions = FALSE, # nolint
                           level = "reg", dir = ".", debug = FALSE, cropTypes = FALSE, threshold = 0.05, progress = TRUE) {


  if (level %in% c("cell", "reg", "grid", "iso")) {
    kcr <- findset("kcr")
    harvestDetail <- production(gdx, products = "kcr", attributes = "nr", level = level, dir = dir)
    harvest <- dimSums(harvestDetail, dim = c(3))

    resDetail <- collapseNames(ResidueBiomass(gdx, product_aggr = FALSE, attributes = "nr", level = level, dir = dir))
    res <- dimSums(resDetail, dim = 3.2)
    ag <- res[, , "ag"]
    bg <- res[, , "bg"]
    seedDetail <- Seed(gdx, level = level, attributes = "nr", dir = dir)
    seed <- dimSums(seedDetail, dim = 3)
    agRecycling <- dimSums(readGDX(gdx, "ov18_res_ag_recycling", select = list(type = "level"))[, , "nr"],
                           dim = c(3.1, 3.2))
    agRecycling <- gdxAggregate(gdx = gdx, weight = "ResidueBiomass", x = agRecycling, to = level,
                                absolute = TRUE, dir = dir, product_aggr = TRUE, attributes = "nr", plantpart = "ag")

    ash <- dimSums((readGDX(gdx, "ov_res_ag_burn", "ov18_res_ag_burn", select = list(type = "level"),
                            format = "first_found")[, , kcr]
                    * (1 - readGDX(gdx, "f18_res_combust_eff")[, , kcr]))[, , "nr"],
                   dim = 3)
    ash <- gdxAggregate(gdx = gdx, weight = "ResidueBiomass", x = ash, to = level, absolute = TRUE,
                        dir = dir, product_aggr = TRUE, attributes = "nr", plantpart = "ag")

    bgRecycling <- bg
    fixationFreeliving <- dimSums(croparea(gdx, products = "kcr", product_aggr = FALSE, level = level,
                                           dir = dir) * readGDX(gdx, "f50_nr_fix_area"),
                                  dim = 3) + setNames(fallow(gdx = gdx, level = level, dir = dir)
                                                      * readGDX(gdx, "f50_nr_fix_area")[, , "tece"], NULL)

    fixationCrops <- harvestDetail + dimSums(resDetail, dim = 3.1)
    fixationRate <- readGDX(gdx, "f50_nr_fix_ndfa")[, getYears(harvest)]

    if (level %in% c("grid", "iso")) {
      fixationRate <- gdxAggregate(gdx, x = fixationRate, to = level, absolute = FALSE, dir = dir)
    }

    fixationCrops <- dimSums(fixationRate * fixationCrops, dim = 3)

    balanceflow <- readGDX(gdx, "f50_nitrogen_balanceflow")[, getYears(harvest), ]
    balanceflow <- gdxAggregate(gdx = gdx, weight = "land", x = balanceflow,
                                to = level, absolute = TRUE, dir = dir, types = "crop")

    som <- readGDX(gdx, "ov_nr_som_fertilizer", select = list(type = "level"), format = "first_found")
    som <- gdxAggregate(gdx = gdx, weight = "land", x = som, to = level, absolute = TRUE, dir = dir, types = "crop")


    manureConfinement <- readGDX(gdx, "ov_manure_confinement", select = list(type = "level"))[, , "nr"]
    recyclingShare <- readGDX(gdx, "i55_manure_recycling_share")[, , "nr"]
    manureRecycling <- dimSums(manureConfinement * recyclingShare, dim = c(3.2, 3.3))
    manureRecycling <- gdxAggregate(gdx = gdx, weight = "ManureExcretion", x = manureRecycling,
                                    to = level, absolute = TRUE, dir = dir, products = readGDX(gdx, "kli"),
                                    awms = "confinement", agg = "awms")
    manure <- dimSums(manureRecycling, dim = 3)

    croplandgrazing <- dimSums(readGDX(gdx, "ov_manure",
                                       select = list(type = "level"))[, , "stubble_grazing"][, , "nr"],
                               dim = c(3.2, 3.3))
    croplandgrazing <- gdxAggregate(gdx = gdx, weight = "ManureExcretion", x = croplandgrazing,
                                    to = level, absolute = TRUE, dir = dir, products = readGDX(gdx, "kli"),
                                    awms = "stubble_grazing", agg = "awms")
    croplandgrazing <- dimSums(croplandgrazing, dim = 3)

    dep <- readGDX(gdx, "ov50_nr_deposition")[, , "crop"][, , "level"]
    dep <- gdxAggregate(gdx = gdx, weight = "land", x = dep, to = level, absolute = TRUE, dir = dir, types = "crop")

    # dimSums(readGDX(gdx,"ov50_nr_dep_crop",select=list(type="level"))

    fertilizer <- collapseNames(readGDX(gdx, "ov_nr_inorg_fert_reg", format = "first_found",
                                        select = list(type = "level"))[, , "crop"])



    out <- mbind(
      setNames(harvest, "harvest"),
      setNames(ag, "ag"),
      setNames(bg, "bg"),
      setNames(fixationCrops, "fixation_crops"),
      setNames(fixationFreeliving, "fixation_freeliving"),
      setNames(agRecycling, "ag_recycling"),
      setNames(ash, "ag_ash"),
      setNames(bgRecycling, "bg_recycling"),
      setNames(som, "som"),
      setNames(seed, "seed"),
      setNames(manure, "manure_conf"),
      setNames(croplandgrazing, "manure_stubble_grazing"),
      setNames(dep, "deposition"),
      setNames(balanceflow, "balanceflow")
    )

    ### distribution of fertilizer

    if (level %in% c("cell", "grid", "iso")) {
      withdrawals <- dimSums(mbind(
        out[, , c("harvest", "ag", "bg")],
        -out[, , c("seed", "fixation_crops")]
      ), dim = 3)
      organicinputs <- dimSums(out[, , c("fixation_freeliving", "ag_recycling", "ag_ash",
                                         "bg_recycling", "som", "manure_conf", "manure_stubble_grazing",
                                         "deposition", "balanceflow")], dim = 3)


      if (level == "cell") {
        mapping <- readGDX(gdx, "cell")
      } else if (level %in% c("grid", "iso")) {
        clustermapFilepath <- Sys.glob(file.path(dir, "clustermap*.rds"))
        if (length(clustermapFilepath) == 1) {
          mapping <- readRDS(clustermapFilepath)
          colnames(mapping) <- c("grid", "cell", "reg", "iso", "glo")
        } else {
          stop("No mapping for toolFertilizerDistribution found")
        }
        if (level == "grid") {
          mapping <- mapping[, c("reg", "grid")]
        } else if (level == "iso") {
          mapping <- mapping[, c("reg", "iso")]
        }
        names(mapping) <- c("i", "j")
      }

      snupe <- readGDX(gdx, "ov50_nr_eff", "ov_nr_eff", format = "first_found")[, , "level"]
      fert <- toolFertilizerDistribution(iterMax = 200, maxSnupe = 0.85, threshold = threshold,
                                         mapping = mapping, from = "j", to = "i", fertilizer = fertilizer,
                                         snupe = snupe, withdrawals = withdrawals, organicinputs = organicinputs,
                                         progress = progress)

    } else {
      fert <- gdxAggregate(x = fertilizer, gdx = gdx, to = level, absolute = TRUE, dir = dir)
    }

    out <- mbind(out, setNames(fert, "fertilizer"))


    ### surplus and emissions
    out <- mbind(
      out,
      setNames(
        dimSums(out[, , c("harvest", "ag", "bg"), invert = TRUE], dim = 3)
        - dimSums(out[, , c("harvest", "ag", "bg")], dim = 3),
        "surplus"
      )
    )

    if (any(out[, , "surplus"] < 0)) {
      balanceflow <- out[, , "surplus"]
      balanceflow[balanceflow > 0] <- 0
      balanceflow <- -balanceflow
      out[, , "surplus"] <- out[, , "surplus"] + balanceflow
      out[, , "balanceflow"] <- out[, , "balanceflow"] + balanceflow
    }

    if (include_emissions) {
      emissions <- Emissions(gdx, type = c("n2o_n", "nh3_n", "no2_n", "no3_n"), level = "reg", unit = "element",
                             subcategories = TRUE, lowpass = FALSE, inorg_fert_split = TRUE, cumulative = FALSE)
      types <- c("SOM", "man_crop", "resid", "rice", "inorg_fert_crop")
      emissions <- emissions[, , types]
      emissions <- dimSums(emissions, dim = "emis_source")
      emissions <- gdxAggregate(gdx = gdx, x = emissions, weight = dimSums(out[, , "surplus"]), to = level,
                                absolute = TRUE, dir = dir)

      out <- mbind(out, emissions)
    }


    ### error checks

    if (level == "reg") {
      # downwards compatibility:
      if (is.null(readGDX(gdx, "v50_nr_inputs")[[1]])) {
        warning("no error checks performed because model version is outdated")
      } else {
        # withdrawals from gams
        checkOut <- readGDX(gdx, "ov50_nr_withdrawals")[, , "level"]
        # withdrawals from postprocessing
        checkOut2 <- (dimSums(out[, , c("harvest", "ag", "bg")], dim = 3)
                      - dimSums(out[, , c("fixation_crops", "seed")], dim = 3))
        # other form of calculating withdrawals
        checkOut3a <- dimSums(out[, , c("fertilizer", "fixation_freeliving",
                                        "ag_recycling", "ag_ash", "bg_recycling", "som", "manure_conf",
                                        "manure_stubble_grazing", "deposition", "balanceflow")])

        checkOut3b <- readGDX(gdx, "ov_nr_eff", "ov50_nr_eff", format = "first_found")[, , "level"] * checkOut3a
        checkOut3c <- (readGDX(gdx, "ov_nr_eff", "ov50_nr_eff", format = "first_found")[, , "level"]
                       * readGDX(gdx, "ov50_nr_inputs", select = list(type = "level")))
        # other form of calculating withdrawals
        checkOut4 <- (1 - readGDX(gdx, "f50_nr_fix_ndfa")[, getYears(harvest), ]) * (
          (readGDX(gdx, "ov_prod_reg")[, , "level"][, , kcr]
           * collapseNames(readGDX(gdx, "fm_attributes")[, , kcr][, , "nr"]))
          + readGDX(gdx, "ov_res_biomass_ag", select = list(type = "level"))[, , "nr"][, , kcr]
          + readGDX(gdx, "ov_res_biomass_bg", select = list(type = "level"))[, , "nr"][, , kcr]
        ) - dimSums((readGDX(gdx, "ov_dem_seed", select = list(type = "level"))
                     * readGDX(gdx, "fm_attributes")[, , "nr"])[, , kcr], dim = "attributes")
        checkOut5 <- readGDX(gdx, "ov50_nr_inputs", select = list(type = "level"))
        checkOut7 <- out[, , "surplus"]
        checkOut8 <- readGDX(gdx, "ov50_nr_surplus_cropland", format = "first_found", select = list(type = "level"))

        if (sum(abs(dimSums(checkOut, dim = 3) - checkOut2)) > 0.1) {
          warning("Withdrawals from gams and postprocessing dont match")
        }
        if (any(dimSums(checkOut3c, dim = 3) - dimSums(checkOut, dim = 3) < (-10^-5))) {
          warning("Input or withdrawal calculations in gams have changed and postprocessing should be adapted")
        }
        if (max(dimSums(checkOut3c, dim = 3) - dimSums(checkOut, dim = 3)) > 0.5) {
          warning("Inputs exceed withdrawals by more than 0.5 Tg")
        }
        if (any(checkOut3b - dimSums(checkOut, dim = 3) < (-10^-5))) {
          warning("Input calculations in gams have changed and postprocessing should be adapted")
        }
        if (sum(abs(dimSums(checkOut3a, dim = 3) - dimSums(checkOut5, dim = 3))) > 0.1) {
          warning("Input or withdrawal calculations in gams have changed and postprocessing should be adapted")
        }
        if (sum(abs(checkOut - checkOut4)) > 0.1) {
          warning("Withdrawal calculations in gams have changed and postprocessing should be adapted")
        }
        if (sum(abs(checkOut8 - checkOut7)) > 0.1) {
          warning("Surplus in gams and postprocessing dont match")
        }
        if (include_emissions) {
          checkOut9 <- dimSums(out[, , c("n2o_n", "nh3_n", "no2_n", "no3_n")], dim = 3)
          if (any((checkOut7 - checkOut9) < 0)) {
            warning("Emissions exceed surplus. Maybe use rescale realization of 51_nitrogen")
          }
          if (any(((checkOut9 + 0.5 * 10^-10) / (checkOut7 + 10^-10)) > 0.9)) {
            warning("N2 emissions in surplus very low")
          }
        }
        ### End of checks
      }
    } else if (level == "cell") {
      reg <- NitrogenBudget(gdx = gdx, include_emissions = include_emissions, level = "reg")
      diff <- superAggregate(data = out, aggr_type = "sum", level = "reg") - reg

      if (debug) { # nolint
        if (any(diff > 0.2)) {
          print(where(diff > 0)$true)
          warning("cellular and regional aggregates diverge by more than 0.2 Tg N")
        }
      }
    }
    if (cropTypes) {
      weight <- NitrogenBudgetWithdrawals(gdx, kcr = "kcr", level = level, net = TRUE, dir = dir)
      out <- ((out * weight) / dimSums(weight, dim = 3, na.rm = TRUE))
    }
    return(out)
  } else if (level == "glo") {
    out <- NitrogenBudget(gdx, include_emissions = include_emissions, level = "reg")
    out <- setItems(dimSums(out, dim = 1), dim = 1, "GLO")
    if (cropTypes) {
      weight <- NitrogenBudgetWithdrawals(gdx, kcr = "kcr", level = "glo", net = TRUE)
      out <- ((out * weight) / dimSums(weight, dim = 3, na.rm = TRUE))
    }
    return(out)
  } else if (level == "regglo") {
    out <- NitrogenBudget(gdx, include_emissions = include_emissions, level = "reg")
    out <- mbind(out, setItems(dimSums(out, dim = 1), dim = 1, "GLO"))
    if (cropTypes) {
      weight <- NitrogenBudgetWithdrawals(gdx, kcr = "kcr", level = "regglo", net = TRUE)
      out <- ((out * weight) / dimSums(weight, dim = 3, na.rm = TRUE))
    }
    return(out)
  }
}
