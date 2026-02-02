#' @title hourlyLaborCosts
#' @description returns hourly labor costs in agriculture from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation to report employment ("iso", "reg", "glo", or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @return hourly labor costs in agriculture
#' @author Debbora Leip
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- hourlyLaborCosts(gdx)
#' }


hourlyLaborCosts <- function(gdx, level = "reg", file = NULL) {

  # for MAgPIE versions before implementation of employment return NULL
  if (is.null(readGDX(gdx, "p36_hourly_costs_iso", react = "silent"))) {
    hourlyLaborCosts <- NULL
  } else {
    if (level == "iso") {
      hourlyLaborCosts <- readGDX(gdx, "p36_hourly_costs_iso")[, , "scenario", drop = TRUE]
    } else if (level == "reg") {
      hourlyLaborCosts <- readGDX(gdx, "pm_hourly_costs")[, , "scenario", drop = TRUE]
    } else if (level %in% c("regglo", "glo") || isCustomAggregation(level)) {
      hourlyLaborCosts <- readGDX(gdx, "pm_hourly_costs")[, , "scenario", drop = TRUE]
      totalHoursWorked <- totalHoursWorked(gdx, level = "reg")
      hourlyLaborCosts <- superAggregateX(hourlyLaborCosts, aggr_type = "weighted_mean",
                                          level = level, weight = totalHoursWorked)
    } else {
      stop("Spatial aggregation level not available")
    }
    hourlyLaborCosts <- hourlyLaborCosts[, readGDX(gdx, "t"), ]
  }

  out(hourlyLaborCosts, file)
}
