#' @title costs
#' @description reads costs entering the objective function from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or
#'  any other aggregation level defined in superAggregate
#' @param sum total costs (TRUE) or detailed costs (FALSE)
#' @param type either "annuity" (as it enters the objetive function) or "investment" (investment)
#' @return A MAgPIE object containing the goal function costs including investments [million US$05]
#' @author Jan Philipp Dietrich, Markus Bonsch, Misko Stevanovic, Florian Humpenoeder,
#' Edna J. Molina Bacca, Michael Crawford
#' @importFrom gdx readGDX out
#' @importFrom magclass mbind dimSums collapseNames
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- costs(gdx)
#' }
#'
costs <- function(gdx, file = NULL, level = "reg", type = "annuity", sum = TRUE) {

  tmpCost <- function(gdx, name, label) {
    cost <- readGDX(gdx, name, format = "first_found", select = list(type = "level"), react = "quiet")
    if (is.null(cost)) return(NULL)
    cost <- dimSums(cost, dim = 3)
    cost <- superAggregate(cost, aggr_type = "sum", level = "reg")
    dimnames(cost)[[3]] <- label
    return(cost)
  }

  fAn <- 1

  if (type == "investment") {

    intRate <- intRate <- readGDX(gdx, "pm_interest")[, readGDX(gdx, "t"), ]
    t <- getYears(intRate, as.integer = TRUE)
    tStep <- t - c(1990, t[seq_len(length(t))[1:(length(t) - 1)]])
    tSm <- intRate

    for (y in seq_len(length(getYears(tSm)))) {
      tSm[, y, ] <- tStep[y]
    }

    fAn <- (1 + intRate) / (intRate) / tSm

  }

  x <- list(
    tmpCost(gdx, "ov_cost_landcon", "Land Conversion") * fAn,
    tmpCost(gdx, "ov_cost_transp", "Transport"),
    tmpCost(gdx, "ov_nr_inorg_fert_costs", "N Fertilizer"),
    tmpCost(gdx, "ov_p_fert_costs", "P Fertilizer"),
    tmpCost(gdx, "ov_reward_cdr_aff", "Reward for Afforestation") * -1 * fAn,
    tmpCost(gdx, "ov_maccs_costs", "MACCS"),
    tmpCost(gdx, "ov_cost_AEI", "AEI") * fAn,
    tmpCost(gdx, "ov_cost_trade", "Trade"),
    tmpCost(gdx, "ov_cost_timber", "Timber production"),
    tmpCost(gdx, "ov_cost_bioen", "Bioenergy"),
    tmpCost(gdx, c("ov_cost_processing", "ov_processing_costs"), "Processing"),
    tmpCost(gdx, "ov_costs_overrate_cropdiff", "Punishment overrated cropland difference"),
    tmpCost(gdx, "ov_rotation_penalty", "Penalty or tax for violating crop rotations"),
    tmpCost(gdx, "ov_bioenergy_utility", "Reward for producing bioenergy"),
    tmpCost(gdx, "ov_processing_substitution_cost", "Substitution processing"),
    tmpCost(gdx, "ov_costs_additional_mon", "Punishment cost for additionally transported monogastric livst_egg"),
    tmpCost(gdx, "ov_cost_land_transition", "Land transition matrix"),
    tmpCost(gdx, "ov_peatland_emis_cost", "Peatland GHG emisssions"),
    tmpCost(gdx, "ov_cost_hvarea_natveg", "Harvesting natural vegetation"),
    tmpCost(gdx, "ov_cost_bv_loss", "Biodiversity value loss"),
    tmpCost(gdx, "ov_cost_urban",   "Punishment urban deviation"),
    tmpCost(gdx, "ov_water_cost",   "Irrigation water")
  )

  # Input factors
  if (suppressWarnings(!is.null(readGDX(gdx, "ov_cost_prod")))) {
    if (suppressWarnings(is.null(readGDX(gdx, "p38_capital_mobile")))) {
      inputCosts <- tmpCost(gdx, "ov_cost_prod", "Input Factors")

    } else {
      if (type == "annuity") {
        inputCosts <- tmpCost(gdx, "ov_cost_prod", "Input Factors") +
          tmpCost(gdx, "ov_cost_inv", "Input Factors")
      } else if (type == "investment") {
        inputCosts <- tmpCost(gdx, "ov_cost_prod", "Input Factors") +
          (tmpCost(gdx, "ov38_investment_immobile", "Input Factors") +
             tmpCost(gdx, "ov38_investment_mobile", "Input Factors")) / tSm
      }

    }
  } else {
    if (suppressWarnings(is.null(readGDX(gdx, "p38_capital_mobile")))) {
      inputCosts <- tmpCost(gdx, "ov_cost_prod_crop", "Input Factors") +
        tmpCost(gdx, "ov_cost_prod_kres", "Input Factors") +
        tmpCost(gdx, "ov_cost_prod_past", "Input Factors") +
        tmpCost(gdx, "ov_cost_prod_livst", "Input Factors") +
        tmpCost(gdx, "ov_cost_prod_fish", "Input Factors")

    } else {
      if (type == "annuity") {
        inputCosts <- tmpCost(gdx, "ov_cost_prod_crop", "Input Factors") +
          tmpCost(gdx, "ov_cost_prod_kres", "Input Factors") +
          tmpCost(gdx, "ov_cost_prod_past", "Input Factors") +
          tmpCost(gdx, "ov_cost_prod_livst", "Input Factors") +
          tmpCost(gdx, "ov_cost_prod_fish", "Input Factors")

      } else if (type == "investment") {
        inputCosts <- tmpCost(gdx, "ov_cost_prod_kres", "Input Factors") +
          tmpCost(gdx, "ov_cost_prod_past", "Input Factors") +
          tmpCost(gdx, "ov_cost_prod_livst", "Input Factors") +
          tmpCost(gdx, "ov_cost_prod_fish", "Input Factors") +
          setNames(readGDX(gdx, "ov_cost_prod_crop", format = "first_found",
                           select = list(type = "level"), react = "quiet")[, , "labor"], "Input Factors") +
          (tmpCost(gdx, "ov38_investment_immobile", "Input Factors") +
             tmpCost(gdx, "ov38_investment_mobile", "Input Factors")) / tSm

      }


    }
  }

  # Peatland
  if (suppressWarnings(is.null(readGDX(gdx, "ov58_peatland_cost_annuity")))) {
    peatland <- tmpCost(gdx, "ov_peatland_cost", "Peatland")

  } else {
    peatland <- tmpCost(gdx, "ov_peatland_cost", "Peatland") -
      tmpCost(gdx, "ov58_peatland_cost_annuity", "Peatland") +
      tmpCost(gdx, "ov58_peatland_cost_annuity", "Peatland") * fAn
  }

  # Forestry
  if (suppressWarnings(is.null(readGDX(gdx, "ov32_cost_establishment")))) {
    forestry <- tmpCost(gdx, "ov_cost_fore", "Forestry")

  } else {
    forestry <- tmpCost(gdx, "ov_cost_fore", "Forestry") - tmpCost(gdx, "ov32_cost_establishment", "Forestry") +
      tmpCost(gdx, "ov32_cost_establishment", "Forestry") * fAn
  }

  # TC
  if (suppressWarnings(is.null(readGDX(gdx, "ov13_cost_tc")))) {
    technology <- tmpCost(gdx, "ov_tech_cost", "Technology")

  } else {
    technology <- tmpCost(gdx, "ov_tech_cost", "Technology") * fAn
  }

  # GHG emissions


  emisCostOneoff <- readGDX(gdx, "ov56_emission_cost", select = list(type = "level"), react = "silent")
  if (!is.null(emisCostOneoff)) {
    emisOneoff <- readGDX(gdx, "emis_oneoff")
    emisCostOneoff <- emisCostOneoff[, , emisOneoff]
    emisCostOneoff <- dimSums(emisCostOneoff, dim = 3)
  } else {
    costsCellOneoff <- dimSums(superAggregate(collapseNames(
      readGDX(gdx, "ov56_emission_costs_cell_oneoff")[, , "level"]), aggr_type = "sum", level = "reg"), dim = 3)
    costsRegOneoff <- if (is.null(getNames(
      readGDX(gdx, "ov56_emission_costs_reg_oneoff")))) 0 else
        dimSums(readGDX(gdx, "ov56_emission_costs_reg_oneoff")[, , "level"], dim = 3)
    emisCostOneoff <- costsCellOneoff + costsRegOneoff
  }


  emissions <- tmpCost(gdx, "ov_emission_costs", "GHG Emissions") - emisCostOneoff + emisCostOneoff * fAn

  x[[length(x) + 1]] <- inputCosts
  x[[length(x) + 1]] <- peatland
  x[[length(x) + 1]] <- forestry
  x[[length(x) + 1]] <- technology
  x[[length(x) + 1]] <- emissions

  x <- mbind(x)

  if (sum) {
    x <- dimSums(x, dim = 3)
  }

  if (type == "annuity") {
    # check
    if (any(abs(readGDX(gdx, "ov11_cost_reg", select = list(type = "level")) - dimSums(x, dim = 3)) > 1e-6)) {
      warning("Differences between total cost and cost categories detected.
            A newly added cost item in the MAgPIE cost module might be missing
            in the costs R function.")
    }
  } else {
    try(costs(gdx, file = file, level = level, type = "annuity", sum = TRUE))

  }


  # aggregate
  x <- superAggregate(x, aggr_type = "sum", level = level, crop_aggr = sum)

  out(x, file)
}
