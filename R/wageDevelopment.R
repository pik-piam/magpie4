#' @title wageDevelopment
#' @description calculates indicator to describe wage development based on agricultural wages in MAgPIE (hourly labor
#' costs relative to a base year)
#'
#' @export
#'
#' @param gdx GDX file
#' @param baseYear year relative to which the wage development should be calculated
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("iso", "reg", "glo", "regglo")
#' @return MAgPIE object containing indicator on wage development
#' @author Debbora Leip
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- wageDevelopment(gdx)
#' }
#'
wageDevelopment <- function(gdx, baseYear = 2000, file = NULL, level = "regglo") {

  x <- readGDX(gdx, "p36_hourly_costs_iso", react = "silent")[, , "scenario", drop = TRUE]

  if (!is.null(x)) {
    x <- collapseDim(x / x[, baseYear, ], dim = 2.2)
    pop <- population(gdx, level = "iso")
    pop[, , ] <- pop[, baseYear, ]
    x <- x[, getYears(pop), ]
    if (level != "iso") {
      map <- readGDX(gdx, "i_to_iso")
      x <- toolAggregate(x, rel = map, weight = pop, from = "iso", to = "i", dim = 1)
      if (level != "reg") {
        pop <- population(gdx, level = "reg")
        pop[, , ] <- pop[, baseYear, ]
        x <- superAggregate(x, aggr_type = "weighted_mean", weight = pop, level = level)
      }
    }
  }

  out(x, file)
}
