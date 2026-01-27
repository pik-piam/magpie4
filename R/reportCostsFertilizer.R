#' @title reportCostsFertilizer
#' @description reports MAgPIE nitrogen fertilizer costs disaggregated to crop categories
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return magpie object with fertilizer costs
#' @author Debbora Leip
#' @examples
#'   \dontrun{
#'     x <- reportCostsFertilizer(gdx)
#'   }
#' @section Nitrogen fertilizer cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs\|N Fertilizer | million US$2017/yr | Total nitrogen fertilizer costs
#' Costs\|N Fertilizer\|+\|Crops | million US$2017/yr | N fertilizer costs for crops
#' Costs\|N Fertilizer\|+\|Pasture | million US$2017/yr | N fertilizer costs for pasture
#' @md
#' @importFrom magpiesets reporthelper summationhelper
#' @export
reportCostsFertilizer <- function(gdx, level = "regglo") {
  fertilizerCosts <- CostsFertilizer(gdx, level = level)
  # no phosphorus fertilizer costs in MAgPIE

  fertilizerCosts <- reporthelper(fertilizerCosts, dim = 3.1, level_zero_name = "Costs|N Fertilizer", detail = TRUE)
  fertilizerCosts <- summationhelper(fertilizerCosts)

  getNames(fertilizerCosts) <- paste0(getNames(fertilizerCosts), " (million US$2017/yr)")

  return(fertilizerCosts)
}
