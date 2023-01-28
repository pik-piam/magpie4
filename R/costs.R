#' @title costs
#' @description reads costs entering the objective function from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param sum total costs (TRUE) or detailed costs (FALSE)
#' @return A MAgPIE object containing the goal function costs [million US$05]
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
costs <- function(gdx, file = NULL, level = "reg", sum = TRUE) {
  tmpCost <- function(gdx, name, label) {
    cost <- readGDX(gdx, name, format = "first_found", select = list(type = "level"), react = "silent")
    if (is.null(cost)) {
      return(NULL)
    }
    cost <- dimSums(cost, dim = 3)
    cost <- superAggregate(cost, aggr_type = "sum", level = "reg")
    dimnames(cost)[[3]] <- label
    return(cost)
  }

  x <- list(
    tmpCost(gdx, "ov_cost_landcon", "Land Conversion"),
    tmpCost(gdx, "ov_cost_transp", "Transport"),
    tmpCost(gdx, "ov_nr_inorg_fert_costs", "N Fertilizer"),
    tmpCost(gdx, "ov_tech_cost", "TC"),
    tmpCost(gdx, "ov_p_fert_costs", "P Fertilizer"),
    tmpCost(gdx, "ov_emission_costs", "GHG Emissions"),
    tmpCost(gdx, "ov_reward_cdr_aff", "Reward for Afforestation") * -1,
    tmpCost(gdx, "ov_maccs_costs", "MACCS"),
    tmpCost(gdx, "ov_cost_AEI", "AEI"),
    tmpCost(gdx, "ov_cost_trade", "Trade"),
    tmpCost(gdx, "ov_cost_fore", "Forestry"),
    tmpCost(gdx, "ov_cost_timber", "Timber production"),
    tmpCost(gdx, "ov_cost_hvarea_natveg", "Timber harvest natveg"),
    tmpCost(gdx, "ov_cost_bioen", "Bioenergy"),
    tmpCost(gdx, c("ov_cost_processing", "ov_processing_costs"), "Processing"),
    tmpCost(gdx, "ov_costs_overrate_cropdiff", "Punishment overrated cropland difference"),
    tmpCost(gdx, "ov_rotation_penalty", "Penalty or tax for violating crop rotations"),
    tmpCost(gdx, "ov_bioenergy_utility", "Reward for producing bioenergy"),
    tmpCost(gdx, "ov_processing_substitution_cost", "Substitution processing"),
    tmpCost(gdx, "ov_costs_additional_mon", "Punishment cost for additionally transported monogastric livst_egg"),
    tmpCost(gdx, "ov_cost_land_transition", "Land transition matrix"),
    tmpCost(gdx, "ov_peatland_cost", "Peatland"),
    tmpCost(gdx, "ov_peatland_emis_cost", "Peatland GHG emisssions"),
    tmpCost(gdx, "ov_cost_bv_loss", "Biodiversity"),
    tmpCost(gdx, "ov_cost_urban", "Punishment urban deviation")
  )

  if (suppressWarnings(!is.null(readGDX(gdx, "ov_cost_prod")))) {
    if (suppressWarnings(is.null(readGDX(gdx, "ov_cost_inv")))) {
      inputCosts <- tmpCost(gdx, "ov_cost_prod", "Input Factors")
    } else {
      inputCosts <- tmpCost(gdx, "ov_cost_prod", "Input Factors") + tmpCost(gdx, "ov_cost_inv", "Input Factors")
    }
  } else {

    inputCosts <- tmpCost(gdx, "ov_cost_prod_crop", "Input Factors") +
      tmpCost(gdx, "ov_cost_prod_kres", "Input Factors") +
      tmpCost(gdx, "ov_cost_prod_past", "Input Factors") +
      tmpCost(gdx, "ov_cost_prod_livst", "Input Factors") +
      tmpCost(gdx, "ov_cost_prod_fish", "Input Factors")
  }


  x[[length(x) + 1]] <- inputCosts

  x <- mbind(x)

  # check
  if (any(abs(readGDX(gdx, "ov11_cost_reg", select = list(type = "level")) - dimSums(x, dim = 3)) > 1e-6)) {
    warning("Differences between total cost and cost categories detected.
            A newly added cost item in the MAgPIE cost module might be missing
            in the costs R function.")
  }


  if (sum) {
    x <- dimSums(x, dim = 3)
  }

  # aggregate
  x <- superAggregate(x, aggr_type = "sum", level = level, crop_aggr = sum)

  out(x, file)
}
