#' @title wageDevelopment
#' @description calculates indicator to describe wage development based on agricultural wages in MAgPIE (hourly labor
#' costs relative to 2020)
#'
#' @export
#'
#' @param gdx GDX file
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
wageDevelopment <- function(gdx, file = NULL, level = "regglo") {

  x <- readGDX(gdx, "p36_hourly_costs_iso", react = "silent")[, , "scenario", drop = TRUE]

  if (!is.null(x)) {
    x <- collapseDim(x / x[, 2020, ], dim = 2.2)
    pop <- population(gdx, level = "iso")
    pop[, , ] <- pop[, 2020, ]
    x <- x[, getYears(pop), ]
    if (level != "iso") {
      map <- readGDX(gdx, "i_to_iso")
      x <- toolAggregate(x, rel = map, weight = pop, from = "iso", to = "i", dim = 1)
      if (level != "reg") {
        pop <- population(gdx, level = "reg")
        pop[, , ] <- pop[, 2020, ]
        x <- superAggregate(x, aggr_type = "weighted_mean", weight = pop, level = level)
      }
    }
  }

  out(x, file)
}
