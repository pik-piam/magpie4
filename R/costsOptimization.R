#' @title costsOptimization
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
#' @author Edna J. Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom magclass mbind dimSums collapseNames
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- costsOptimization(gdx)
#' }
#'
costsOptimization <- function(gdx, file = NULL, level = "reg", type = "annuity", sum = TRUE) {

  tmp_cost <- function(gdx, name, label) {
    cost <- readGDX(gdx, name, format = "first_found", select = list(type = "level"), react = "quiet")
    if (is.null(cost)) return(NULL)
    cost <- dimSums(cost, dim = 3)
    cost <- superAggregate(cost, aggr_type = "sum", level = "reg")
    dimnames(cost)[[3]] <- label
    return(cost)
  }

  f_an <- 1

  if (type == "investment") {

    int_rate <- int_rate <- readGDX(gdx, "pm_interest")[, readGDX(gdx, "t"), ]
    t <- getYears(int_rate, as.integer = TRUE)
    t_step <- c(t[seq_len(length(t))[2:length(t)]], 2110) - t
    t_sm <- int_rate

    for (y in seq_len(length(getYears(t_sm)))) {
      t_sm[, y, ] <- t_step[y]
    }

    f_an <- (1 + int_rate) / (int_rate) / t_sm

  }

  x <- list(tmp_cost(gdx, "ov_cost_landcon", "Land Conversion") * f_an,
    tmp_cost(gdx, "ov_cost_transp", "Transport"),
    tmp_cost(gdx, "ov_nr_inorg_fert_costs", "N Fertilizer"),
    tmp_cost(gdx, "ov_p_fert_costs", "P Fertilizer"),
    tmp_cost(gdx, "ov_reward_cdr_aff", "Reward for Afforestation") * -1 * f_an,
    tmp_cost(gdx, "ov_maccs_costs", "MACCS"),
    tmp_cost(gdx, "ov_cost_AEI", "AEI") * f_an,
    tmp_cost(gdx, "ov_cost_trade", "Trade"),
    tmp_cost(gdx, "ov_cost_timber", "Timber production"),
    # tmp_cost(gdx,"ov_cost_bioen","Bioenergy"),#?not in magpie
    tmp_cost(gdx, c("ov_cost_processing", "ov_processing_costs"), "Processing"),
    # tmp_cost(gdx,"ov_costs_overrate_cropdiff","Punishment overrated cropland difference"),#?not in magpie
    tmp_cost(gdx, "ov_bioenergy_utility", "Reward for producing bioenergy"),
    tmp_cost(gdx, "ov_processing_substitution_cost", "Substitution processing"),
    tmp_cost(gdx, "ov_costs_additional_mon", "Punishment cost for additionally transported monogastric livst_egg"),
    tmp_cost(gdx, "ov_cost_land_transition", "Land transition matrix"),
    tmp_cost(gdx, "ov_peatland_emis_cost", "Peatland GHG emisssions"),
    tmp_cost(gdx, "ov_cost_hvarea_natveg", "Harvesting natural vegetation"),
    tmp_cost(gdx, "ov_cost_bv_loss", "Biodiversity value loss")
  )

  # Input factors
  if (suppressWarnings(is.null(readGDX(gdx, "ov_cost_inv")))) {
    input_costs <- tmp_cost(gdx, "ov_cost_prod", "Input Factors")

  } else {
    input_costs <- tmp_cost(gdx, "ov_cost_prod", "Input Factors") + tmp_cost(gdx, "ov_cost_inv", "Input Factors")
  }

  # Peatland
  if (suppressWarnings(is.null(readGDX(gdx, "ov58_peatland_cost_annuity")))) {
    peatland <- tmp_cost(gdx, "ov_peatland_cost", "Peatland")

  } else {
    peatland <- tmp_cost(gdx, "ov_peatland_cost", "Peatland") -
      tmp_cost(gdx, "ov58_peatland_cost_annuity", "Peatland") +
      tmp_cost(gdx, "ov58_peatland_cost_annuity", "Peatland") * f_an
  }

  # Forestry
  if (suppressWarnings(is.null(readGDX(gdx, "ov32_cost_establishment")))) {
    forestry <- tmp_cost(gdx, "ov_cost_fore", "Forestry")

  } else {
    forestry <- tmp_cost(gdx, "ov_cost_fore", "Forestry") - tmp_cost(gdx, "ov32_cost_establishment", "Forestry") +
      tmp_cost(gdx, "ov32_cost_establishment", "Forestry") * f_an
  }

  # TC
  if (suppressWarnings(is.null(readGDX(gdx, "ov13_cost_tc")))) {
    technology <- tmp_cost(gdx, "ov_tech_cost", "Technology")

  } else {
    technology <- tmp_cost(gdx, "ov_tech_cost", "Technology") * f_an
  }

  # GHG emissions

  costs_cell_oneoff <- dimSums(superAggregate(collapseNames(
    readGDX(gdx, "ov56_emission_costs_cell_oneoff")[, , "level"]), aggr_type = "sum", level = "reg"), dim = 3)
  costs_reg_oneoff <- if (is.null(getNames(
    readGDX(gdx, "ov56_emission_costs_reg_oneoff")))) 0 else 
      dimSums(readGDX(gdx, "ov56_emission_costs_reg_oneoff")[, , "level"], dim = 3)

  emissions <- tmp_cost(gdx, "ov_emission_costs", "GHG Emissions") - (
    costs_reg_oneoff +
      costs_cell_oneoff) * f_an +
    costs_reg_oneoff +
    costs_cell_oneoff

  x[[length(x) + 1]] <- input_costs
  x[[length(x) + 1]] <- peatland
  x[[length(x) + 1]] <- forestry
  x[[length(x) + 1]] <- technology
  x[[length(x) + 1]] <- emissions

  x <- mbind(x)

  if (!as.numeric(readGDX(gdx, "s73_timber_demand_switch"))) x[, , "Timber production"] <- 0

  if (sum) {
    x <- dimSums(x, dim = 3)
  }


  # aggregate
  x <- superAggregate(x, aggr_type = "sum", level = level, crop_aggr = sum)

  out(x, file)
}
