#' @title reportCostsTrade
#' @description Reports MAgPIE bilateral trade cost components
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param sum whether to sum across subcategories
#' @return A MAgPIE object containing values related to trade costs (million US$2017/yr)
#' @author David M Chen
#' @importFrom magpiesets reporthelper summationhelper
#' @importFrom magclass mbind getNames<-
#' @export

reportCostsTrade <- function(gdx, level = "regglo", sum = FALSE) {

  tariff <- readGDX(gdx, "ov_cost_trade_tariff", select = list(type = "level"), react = "silent")
  margin <- readGDX(gdx, "ov_cost_trade_margin", select = list(type = "level"), react = "silent")
  feasibility <- readGDX(gdx, "ov_cost_trade_feasibility", select = list(type = "level"), react = "silent")

  if (is.null(tariff) && is.null(margin) && is.null(feasibility)) {
    return(NULL)
  }

  tariff <- gdxAggregate(gdx, dimSums(tariff, dim = 3), to = level, absolute = TRUE)
  margin <- gdxAggregate(gdx, dimSums(margin, dim = 3), to = level, absolute = TRUE)
  feasibility <- gdxAggregate(gdx, dimSums(feasibility, dim = 3), to = level, absolute = TRUE)

  getNames(tariff)      <- "Costs|Trade|+|Tariffs (million US$2017/yr)"
  getNames(margin)      <- "Costs|Trade|+|Margins (million US$2017/yr)"
  getNames(feasibility) <- "Costs|Trade|+|Imports for feasibility (million US$2017/yr)"

  x <- mbind(tariff, margin, feasibility)
  if (sum) {
  x <- dimSums(x, dim = 3)
  getNames(x) <- "Costs|Trade (million US$2017/yr)"
  }

  x <- summationhelper(x, excludeLevels = 1)

  return(x)
}
