#' @title reportCostsFertilizer
#' @description reports MAgPIE nitrogen fertilizer costs disaggregated to crop categories
#'
#' @param gdx GDX file
#' @return magpie object with fertilizer costs
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportCostsFertilizer(gdx)
#'   }
#'
#' @importFrom magpiesets reporthelper summationhelper
#'
reportCostsFertilizer <- function(gdx) {
  fertilizerCosts <- CostsFertilizer(gdx, level = "regglo")
  totalCosts <- setNames(dimSums(fertilizerCosts, dim = 3), "Costs|Fertilizer")
  # no phosphorus fertilizer costs in MAgPIE

  fertilizerCosts <- reporthelper(fertilizerCosts, dim = 3.1, level_zero_name = "Costs|Fertilizer", detail = TRUE)
  fertilizerCosts <- summationhelper(fertilizerCosts)
  fertilizerCosts <- mbind(totalCosts, fertilizerCosts)

  getNames(fertilizerCosts) <- paste0(getNames(fertilizerCosts), " (million US$05/yr)")

  return(fertilizerCosts)
}
