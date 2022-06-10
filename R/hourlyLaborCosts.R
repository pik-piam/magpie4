#' @title hourlyLaborCosts
#' @description returns hourly labor costs in agriculture from MAgPIE results
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
      hourlyLaborCosts <- readGDX(gdx, "p36_hourly_costs_iso")
    } else if (level == "reg") {
      hourlyLaborCosts <- readGDX(gdx, "p36_hourly_costs")
    } else if (level == "glo") {
      hourlyLaborCosts <- readGDX(gdx, "p36_hourly_costs")
      totalHoursWorked <- totalHoursWorked(gdx, level = "reg")
      hourlyLaborCosts <- superAggregate(hourlyLaborCosts, aggr_type = "weighted_mean",
                                         level = "glo", weight = totalHoursWorked)
    } else if (level == "regglo") {
      hourlyLaborCosts <- mbind(hourlyLaborCosts(gdx, level = "reg"), hourlyLaborCosts(gdx, level = "glo"))
    } else {
      stop("Spatial aggregation level not available")
    }
  }

  out(hourlyLaborCosts, file)
}
