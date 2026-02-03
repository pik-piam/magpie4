#' @title reportLivestockShare
#' @description reports the share of livestock products (including fish) in total calorie food supply
#'
#' @export
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return per-capita calories as MAgPIE object (kcal/cap/day)
#' @author Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportLivestockShare(gdx)
#'   }
#'
#' @section Livestock share variables:
#' Name | Unit | Meta
#' ---|---|---
#' Nutrition\|Dietary Composition\|Livestock Share | kcal/kcal | Share of livestock products (incl. fish) in total calorie supply
#' @md
reportLivestockShare <- function(gdx, level = "regglo") {
  out <- Kcal(gdx, level = level, products = "kall", product_aggr = FALSE)
  kap <- findset("kap")

  l <- dimSums(out[, , kap], dim = 3.1)
  weight <- dimSums(out, dim = 3.1)
  out <- l / weight

  getNames(out) <- "Nutrition|Dietary Composition|Livestock Share (kcal/kcal)"

  return(out)
}
