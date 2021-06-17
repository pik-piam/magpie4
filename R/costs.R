#' @title costs
#' @description reads costs entering the objective function from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param sum total costs (TRUE) or detailed costs (FALSE)
#' @return A MAgPIE object containing the goal function costs [million US$05]
#' @author Jan Philipp Dietrich, Markus Bonsch, Misko Stevanovic, Florian Humpenoeder, Edna J. Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom magclass mbind dimSums collapseNames
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- costs(gdx)
#' }
#'
costs <- function(gdx, file = NULL, level = "reg", sum = TRUE) {
  tmp_cost <- function(gdx, name, label) {
    cost <- readGDX(gdx, name, format = "first_found", select = list(type = "level"), react = "quiet")
    if (is.null(cost)) {
      return(NULL)
    }
    cost <- dimSums(cost, dim = 3)
    cost <- superAggregate(cost, aggr_type = "sum", level = "reg")
    dimnames(cost)[[3]] <- label
    return(cost)
  }

  x <- list(
    tmp_cost(gdx, "ov_cost_landcon", "Land Conversion"),
    tmp_cost(gdx, "ov_cost_transp", "Transport"),
    tmp_cost(gdx, "ov_nr_inorg_fert_costs", "N Fertilizer"),
    tmp_cost(gdx, "ov_p_fert_costs", "P Fertilizer"),
    tmp_cost(gdx, "ov_emission_costs", "GHG Emissions"),
    tmp_cost(gdx, "ov_reward_cdr_aff", "Reward for Afforestation") * -1,
    tmp_cost(gdx, "ov_maccs_costs", "MACCS"),
    tmp_cost(gdx, "ov_cost_AEI", "AEI"),
    tmp_cost(gdx, "ov_cost_trade", "Trade"),
    tmp_cost(gdx, "ov_cost_fore", "Forestry"),
    tmp_cost(gdx, "ov_cost_timber", "Timber production"),
    tmp_cost(gdx, "ov_cost_bioen", "Bioenergy"),
    tmp_cost(gdx, c("ov_cost_processing", "ov_processing_costs"), "Processing"),
    tmp_cost(gdx, "ov_costs_overrate_cropdiff", "Punishment overrated cropland difference"),
    tmp_cost(gdx, "ov_bioenergy_utility", "Reward for producing bioenergy"),
    tmp_cost(gdx, "ov_processing_substitution_cost", "Substitution processing"),
    tmp_cost(gdx, "ov_costs_additional_mon", "Punishment cost for additionally transported monogastric livst_egg"),
    tmp_cost(gdx, "ov_cost_land_transition", "Land transition matrix"),
    tmp_cost(gdx, "ov_peatland_cost", "Peatland"),
    tmp_cost(gdx, "ov_peatland_emis_cost", "Peatland GHG emisssions")
  )

  if (suppressWarnings(is.null(readGDX(gdx, "ov_cost_inv")))) {
    input_costs <- tmp_cost(gdx, "ov_cost_prod", "Input Factors")
  } else {
    input_costs <- tmp_cost(gdx, "ov_cost_prod", "Input Factors") + tmp_cost(gdx, "ov_cost_inv", "Input Factors")
  }

  x[[length(x) + 1]] <- input_costs

  x <- mbind(x)

  if (!as.numeric(readGDX(gdx, "s73_timber_demand_switch", "sm_timber_demand_switch"))) x[, , "Timber production"] <- 0

  if (sum) {
    x <- dimSums(x, dim = 3)
  }


  # aggregate
  x <- superAggregate(x, aggr_type = "sum", level = level, crop_aggr = sum)

  out(x, file)
}
