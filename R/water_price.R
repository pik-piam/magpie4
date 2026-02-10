#' @title water_price
#' @description reads water prices from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level spatial level of aggregation: "cell" (cellular), "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param weight For determining weights to use for generating water prices at levels beyond 'cellular'. Takes "value" and "quantity". "value" sums regional weights by value of water per cluster, "quantity" sums regional weight by qty of water per cluster
#' @param index FALSE (default) or TRUE
#' @param index_baseyear baseyear to use for index calculation (only used if index=TRUE)
#' @param digits integer. For rounding of the return values
#' @return A MAgPIE object containing the water shadow prices (US Dollar/cubic metre).
#' @author Markus Bonsch, Vartika Singh, Miodrag Stevanovic
#' @examples
#'
#'   \dontrun{
#'     x <- water_price(gdx)
#'   }
#'

water_price <- function(gdx, file = NULL, level = "reg", weight = "value", index = FALSE, index_baseyear = 2005, digits = 4) {

  #cellular level water price
  p_water_cell <- readGDX(gdx, "oq43_water", "oq_water", format = "first_found")[, , "marginal"]
  if (is.null(p_water_cell)) {
    warning("Water shadow prices cannot be calculated as needed data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }

  p_water_cell <- abs(p_water_cell)

  #cellular level water quantity
  q_water_cell <- setNames(readGDX(gdx, "ov_watdem", "ov43_watdem", "ovm_watdem", format = "first_found")[, , "agriculture.level"], NULL)

  if (is.null(q_water_cell)) {
    warning("Water quantities demanded cannot be calculated as needed data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }

  #regional weights
  if (weight == "value") {
    r_weight <- p_water_cell * q_water_cell
  } else if (weight == "quantity") {
    r_weight <- q_water_cell
  }

  if (level == "cell") {
    water <- p_water_cell
  } else if (level %in% c("reg", "regglo", "glo") || isCustomAggregation(level)) {
    water <- superAggregateX(p_water_cell, level = level, weight = r_weight, aggr_type = "weighted_mean", crop_aggr = FALSE)
  }

  if (index) {
    # check if the baseyear is contained in the gdx
    if (!index_baseyear %in% getYears(water)) {
      water <- time_interpolate(water, index_baseyear, integrate_interpolated_years = TRUE)
    }
    water <- water / setYears(water[, index_baseyear, ], NULL) * 100
  }
  water <- as.magpie(round(water, digits))
  out(water, file)
}
