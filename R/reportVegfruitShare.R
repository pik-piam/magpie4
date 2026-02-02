#' @title reportVegfruitShare
#' @description reports the share of livestock products (including fish) in total calorie food supply
#'
#' @export
#'
#' @param gdx GDX file
#' @return per-capita calories as MAgPIE object (kcal/cap/day)
#' @author Benjamin Leon Bodirsky
#' @examples
#'   \dontrun{
#'     x <- reportLivestockShare(gdx)
#'   }
#'
#' @section Vegfruit share variables:
#' Name | Unit | Meta
#' ---|---|---
#' Nutrition\|Dietary Composition\|Vegetables Fruits Nuts Share | kcal/kcal | Share of vegetables, fruits, and nuts in total calorie supply
#' @md
reportVegfruitShare <- function(gdx, level = "regglo") {
  out <- Kcal(gdx, level = level, products = "kall", product_aggr = FALSE)

  l <- dimSums(out[, , "others"], dim = 3.1)
  weight <- dimSums(out, dim = 3.1)
  out <- l / weight

  getNames(out) <- "Nutrition|Dietary Composition|Vegetables Fruits Nuts Share (kcal/kcal)"

  return(out)
}
