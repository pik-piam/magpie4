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
#' @export
#'
reportCostsFertilizer <- function(gdx) {
  fertilizerCosts <- CostsFertilizer(gdx, level = "regglo")
  # no phosphorus fertilizer costs in MAgPIE

  fertilizerCosts <- reporthelper(fertilizerCosts, dim = 3.1, level_zero_name = "Costs|N Fertilizer", detail = TRUE)
  fertilizerCosts <- summationhelper(fertilizerCosts)

  getNames(fertilizerCosts) <- paste0(getNames(fertilizerCosts), " (million US$2017/yr)")

  return(fertilizerCosts)
}
