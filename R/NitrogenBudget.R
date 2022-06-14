#' @title NitrogenBudget
#' @description calculates projections of Nitrogen Budgets for Croplands (Tg Nr per) from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param include_emissions TRUE also divides the N surplus into different emissions
#' @param level aggregation level, reg, glo or regglo, cell or grid
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param debug debug mode TRUE makes some consistency checks between estimates for different resolutions.
#' @author Benjamin Leon Bodirsky, Michael Crawford, Edna J. Molina Bacca
#' @importFrom magpiesets findset
#' @importFrom madrat toolAggregate
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom gdx readGDX
#' @importFrom luscale superAggregate
#' @importFrom mstools toolFertilizerDistribution
#' @examples
#' \dontrun{
#' x <- NitrogenBudget(gdx)
#' }
#'
NitrogenBudget <- function(gdx, include_emissions = FALSE, level = "reg", dir = ".", debug = FALSE) {


  if (level %in% c("cell", "reg", "grid")) {
    kcr <- findset("kcr")
    harvest_detail <- production(gdx, products = "kcr", attributes = "nr", level = level, dir = dir)
    harvest <- dimSums(harvest_detail, dim = c(3))

    res_detail <- collapseNames(ResidueBiomass(gdx, product_aggr = F, attributes = "nr", level = level, dir = dir))
    res <- dimSums(res_detail, dim = 3.2)
    ag <- res[, , "ag"]
    bg <- res[, , "bg"]
    seed_detail <- Seed(gdx, level = level, attributes = "nr", dir = dir)
    seed <- dimSums(seed_detail, dim = 3)
    ag_recycling <- dimSums(readGDX(gdx, "ov18_res_ag_recycling", select = list(type = "level"))[, , "nr"], dim = c(3.1, 3.2))
    ag_recycling <- gdxAggregate(gdx = gdx, weight = "ResidueBiomass", x = ag_recycling, to = level, absolute = TRUE, dir = dir, product_aggr = T, attributes = "nr", plantpart = "ag")

    ash <- dimSums((readGDX(gdx, "ov_res_ag_burn", "ov18_res_ag_burn", select = list(type = "level"), format = "first_found")[, , kcr] * (1 - readGDX(gdx, "f18_res_combust_eff")[, , kcr]))[, , "nr"], dim = 3)
    ash <- gdxAggregate(gdx = gdx, weight = "ResidueBiomass", x = ash, to = level, absolute = TRUE, dir = dir, product_aggr = T, attributes = "nr", plantpart = "ag")

    bg_recycling <- bg
    fixation_freeliving <- dimSums(
      croparea(gdx, products = "kcr", product_aggr = FALSE, level = level, dir = dir) * readGDX(gdx, "f50_nr_fix_area"),
      dim = 3) + setNames(fallow(gdx=gdx,level=level, dir=dir) * readGDX(gdx, "f50_nr_fix_area")[,,"tece"],NULL)

    fixation_crops <- harvest_detail + dimSums(res_detail, dim = 3.1)
    fixation_rate <- readGDX(gdx, "f50_nr_fix_ndfa")[, getYears(harvest)]

    if (level == "grid") {
      fixation_rate <- gdxAggregate(gdx, x = fixation_rate, to = "grid", absolute = FALSE, dir = dir)
    }

    fixation_crops <- dimSums(fixation_rate * fixation_crops, dim = 3)

    balanceflow <- readGDX(gdx, "f50_nitrogen_balanceflow")[, getYears(harvest), ]
    balanceflow <- gdxAggregate(gdx = gdx, weight = "land", x = balanceflow, to = level, absolute = TRUE, dir = dir, types = "crop")

    som <- readGDX(gdx, "ov_nr_som_fertilizer", select = list(type = "level"), format = "first_found")
    som <- gdxAggregate(gdx = gdx, weight = "land", x = som, to = level, absolute = TRUE, dir = dir, types = "crop")


    manure_confinement <- readGDX(gdx, "ov_manure_confinement", select = list(type = "level"))[, , "nr"]
    recycling_share <- readGDX(gdx, "i55_manure_recycling_share")[, , "nr"]
    manure_recycling <- dimSums(manure_confinement * recycling_share, dim = c(3.2, 3.3))
    manure_recycling <- gdxAggregate(gdx = gdx, weight = "ManureExcretion", x = manure_recycling, to = level, absolute = TRUE, dir = dir, products = readGDX(gdx, "kli"), awms = "confinement", agg = "awms")
    manure <- dimSums(manure_recycling, dim = 3)

    croplandgrazing <- dimSums(readGDX(gdx, "ov_manure", select = list(type = "level"))[, , "stubble_grazing"][, , "nr"], dim = c(3.2, 3.3))
    croplandgrazing <- gdxAggregate(gdx = gdx, weight = "ManureExcretion", x = croplandgrazing, to = level, absolute = TRUE, dir = dir, products = readGDX(gdx, "kli"), awms = "stubble_grazing", agg = "awms")
    croplandgrazing <- dimSums(croplandgrazing, dim = 3)

    dep <- readGDX(gdx, "ov50_nr_deposition")[, , "crop"][, , "level"]
    dep <- gdxAggregate(gdx = gdx, weight = "land", x = dep, to = level, absolute = TRUE, dir = dir, types = "crop")

    # dimSums(readGDX(gdx,"ov50_nr_dep_crop",select=list(type="level"))

    fertilizer <- collapseNames(readGDX(gdx, "ov_nr_inorg_fert_reg", format = "first_found", select = list(type = "level"))[, , "crop"])



    out <- mbind(
      setNames(harvest, "harvest"),
      setNames(ag, "ag"),
      setNames(bg, "bg"),
      # setNames(fertilizer,"fertilizer"),
      setNames(fixation_crops, "fixation_crops"),
      setNames(fixation_freeliving, "fixation_freeliving"),
      setNames(ag_recycling, "ag_recycling"),
      setNames(ash, "ag_ash"),
      setNames(bg_recycling, "bg_recycling"),
      setNames(som, "som"),
      setNames(seed, "seed"),
      setNames(manure, "manure"),
      setNames(croplandgrazing, "manure_stubble_grazing"),
      setNames(dep, "deposition"),
      setNames(balanceflow, "balanceflow")
    )

    ### distribution of fertilizer

    if (level %in% c("cell", "grid")) {
      withdrawals <- dimSums(mbind(
        out[, , c("harvest", "ag", "bg")],
        -out[, , c("seed", "fixation_crops")]
      ), dim = 3)
      organicinputs <- dimSums(out[, , c("fixation_freeliving", "ag_recycling", "ag_ash",
                       "bg_recycling", "som", "manure", "manure_stubble_grazing",
                       "deposition", "balanceflow")], dim = 3)

      SNUpE <- readGDX(gdx, "ov50_nr_eff", "ov_nr_eff")[, , "level"]

      if (level == "cell") {
        mapping <- readGDX(gdx, "cell")
      } else if (level == "grid") {
        mapping <- readRDS(Sys.glob(file.path(dir, "clustermap*.rds")))[, c("region", "cell")]
        names(mapping) <- c("i", "j")
      }

      max_snupe <- 0.85
      threshold <- 0.05
      fert <- toolFertilizerDistribution(iteration_max = 200, max_snupe = max_snupe, threshold = threshold,
                                      mapping = mapping, from = "j", to = "i", fertilizer = fertilizer, SNUpE = SNUpE,
                                      withdrawals = withdrawals, organicinputs = organicinputs)

    } else {
      fert <- gdxAggregate(x = fertilizer, gdx = gdx, to = level, absolute = T, dir = dir)
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

    if (any(out[,,"surplus"] < 0)) {
      warning("due to non-iteration of fertilizer distribution, residual fertilizer deficit is moved to balanceflow.")
      balanceflow <- out[, , "surplus"]
      balanceflow[balanceflow > 0] <- 0
      balanceflow <- -balanceflow
      out[, , "surplus"] <- out[, , "surplus"] + balanceflow
      out[, , "balanceflow"] <- out[, , "balanceflow"] + balanceflow
    }

    if (include_emissions) {
      emissions <- Emissions(gdx, type = c("n2o_n", "nh3_n", "no2_n", "no3_n"), level = "reg", unit = "element", subcategories = TRUE, lowpass = FALSE, inorg_fert_split = TRUE, cumulative = FALSE)
      types <- c("SOM", "man_crop", "resid", "rice", "inorg_fert_crop")
      emissions <- emissions[, , types]
      emissions <- dimSums(emissions, dim = "emis_source")
      emissions <- gdxAggregate(gdx = gdx, x = emissions, weight = dimSums(out[, , "surplus"]), to = level, absolute = TRUE, dir = dir)

      out <- mbind(out, emissions)
    }


    ### error checks

    if (level == "reg") {

      # downwards compatibility:
      if (is.null(readGDX(gdx, "v50_nr_inputs")[[1]])) {
        warning("no error checks performed because model version is outdated")
      } else {

        # withdrawals from gams
        check_out <- readGDX(gdx, "ov50_nr_withdrawals")[, , "level"]
        # withdrawals from postprocessing
        check_out2 <- dimSums(out[, , c("harvest", "ag", "bg")], dim = 3) - dimSums(out[, , c("fixation_crops", "seed")], dim = 3)
        # other form of calculating withdrawals
        check_out3a <- dimSums(
          out[, , c(
            "fertilizer", "fixation_freeliving",
            "ag_recycling", "ag_ash", "bg_recycling", "som", "manure",
            "manure_stubble_grazing", "deposition", "balanceflow")]
        )

        check_out3b <- readGDX(gdx, "ov_nr_eff", "ov50_nr_eff")[, , "level"] * check_out3a
        check_out3c <- readGDX(gdx, "ov_nr_eff", "ov50_nr_eff")[, , "level"] * readGDX(gdx, "ov50_nr_inputs", select = list(type = "level"))
        # other form of calculating withdrawals
        check_out4 <- (1 - readGDX(gdx, "f50_nr_fix_ndfa")[, getYears(harvest), ]) * (
            readGDX(gdx, "ov_prod_reg")[, , "level"][, , kcr] * collapseNames(readGDX(gdx, "fm_attributes")[, , kcr][, , "nr"])
            + readGDX(gdx, "ov_res_biomass_ag", select = list(type = "level"))[, , "nr"][, , kcr]
            + readGDX(gdx, "ov_res_biomass_bg", select = list(type = "level"))[, , "nr"][, , kcr]
          ) - dimSums((readGDX(gdx, "ov_dem_seed", select = list(type = "level")) * readGDX(gdx, "fm_attributes")[, , "nr"])[, , kcr], dim = "attributes")
        check_out5 <- readGDX(gdx, "ov50_nr_inputs", select = list(type = "level"))
        check_out6 <- readGDX(gdx, "ov50_nr_surplus_cropland", select = list(type = "level"))
        check_out7 <- out[, , "surplus"]
        check_out8 <- readGDX(gdx, "ov50_nr_surplus_cropland", format = "first_found", select = list(type = "level"))

        if (sum(abs(dimSums(check_out, dim = 3) - check_out2)) > 0.1) {
warning("Withdrawals from gams and postprocessing dont match")
}
        if (any(dimSums(check_out3c, dim = 3) - dimSums(check_out, dim = 3) < (-10^-5))) {
warning("Input or withdrawal calculations in gams have changed and postprocessing should be adapted")
}
        if (max(dimSums(check_out3c, dim = 3) - dimSums(check_out, dim = 3)) > 0.5) {
warning("Inputs exceed withdrawals by more than 0.5 Tg")
}
        if (any(check_out3b - dimSums(check_out, dim = 3) < (-10^-5))) {
warning("Input calculations in gams have changed and postprocessing should be adapted")
}
        if (sum(abs(dimSums(check_out3a, dim = 3) - dimSums(check_out5, dim = 3))) > 0.1) {
warning("Input or withdrawal calculations in gams have changed and postprocessing should be adapted")
}
        if (sum(abs(check_out - check_out4)) > 0.1) {
warning("Withdrawal calculations in gams have changed and postprocessing should be adapted")
}
        if (sum(abs(check_out8 - check_out7)) > 0.1) {
warning("Surplus in gams and postprocessing dont match")
}
        if (include_emissions) {
          check_out9 <- dimSums(out[, , c("n2o_n", "nh3_n", "no2_n", "no3_n")], dim = 3)
          if (any((check_out7 - check_out9) < 0)) {
warning("Emissions exceed surplus. Maybe use rescale realization of 51_nitrogen")
}
          if (any(((check_out9 + 0.5 * 10^-10) / (check_out7 + 10^-10)) > 0.9)) {
warning("N2 emissions in surplus very low")
}
        }
      ### End of checks
      }
    } else if (level == "cell") {
      reg <- NitrogenBudget(gdx = gdx, include_emissions = include_emissions, level = "reg")
      diff <- superAggregate(data = out, aggr_type = "sum", level = "reg") - reg

      if (debug) {
        if (any(diff > 0.2)) {
          print(where(diff > 0)$true)
          warning("cellular and regional aggregates diverge by more than 0.2 Tg N")
        }
      }
    }
    return(out)
  } else if (level == "glo") {
    out <- NitrogenBudget(gdx, include_emissions = include_emissions, level = "reg")
    out <- setItems(dimSums(out, dim = 1), dim = 1, "GLO")
  } else if (level == "regglo") {
    out <- NitrogenBudget(gdx, include_emissions = include_emissions, level = "reg")
    out <- mbind(out, setItems(dimSums(out, dim = 1), dim = 1, "GLO"))
    return(out)
  }

}
