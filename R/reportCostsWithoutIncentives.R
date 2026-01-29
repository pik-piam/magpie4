#' @title reportCostsWithoutIncentives
#' @description reports Costs Without Incentives
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return magpie object
#' @author David Chen
#' @examples
#' \dontrun{
#' x <- reportCostsWithoutIncentives(gdx)
#' }
#'
#'
#' @section Costs without incentives variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs Accounting\|Costs without incentives | million US$2017/yr | Total costs excluding policy incentives
#' @md
#' @export
reportCostsWithoutIncentives <- function(gdx, level = "regglo") {

  costWoInc <- CostsWithoutIncentives(gdx, level = level)
  getNames(costWoInc) <- paste0("Costs Accounting|Costs without incentives",
                                " (million US$2017/yr)")

  return(costWoInc)
}
