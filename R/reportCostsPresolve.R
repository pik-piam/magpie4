#' @title reportCostsPresolve
#' @description reports MAgPIE costs
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return consumption value as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#'   \dontrun{
#'     x <- reportCostsPresolve(gdx)
#'   }
#'
#' @section Presolve cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs\|PreSolve\|Total | million US$2017 | Cumulative costs from presolve phase
#' @md
#' @export
reportCostsPresolve <- function(gdx, level = "regglo") {

  a <- costsPresolve(gdx, level = level)
  if (!is.null(a)) {
    getNames(a) <- "Costs|PreSolve|Total (million US$2017)"
  }

  return(a)
}