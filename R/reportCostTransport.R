#' @title reportCostTransport
#' @description reports MAgPIE costs
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return consumption value as MAgPIE object Unit: see names
#' @author David Chen
#' @examples
#'   \dontrun{
#'     x <- reportCostTransport(gdx)
#'   }
#'
#' @section Transport cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs\|Transport | million US$2017/yr | Total transport costs for agricultural commodities
#' Costs\|Transport\|+\|Crops | million US$2017/yr | Transport costs for crop products
#' Costs\|Transport\|+\|Livestock products | million US$2017/yr | Transport costs for livestock products
#' @md
#' @importFrom magpiesets reporthelper summationhelper
#' @export

reportCostTransport <- function(gdx, level = level) {

  a <- CostTransport(gdx, level = level, sum = FALSE)

  a <- reporthelper(a, dim = 3.1, level_zero_name = "Costs|Transport", detail = FALSE)
  a <- summationhelper(a)

  getNames(a) <- paste0(getNames(a), " (million US$2017/yr)")

  return(a)
}
