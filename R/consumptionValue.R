#' @title consumptionValue
#' @description calculates consumption value of different types based on a MAgPIE gdx file.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean, default TRUE)
#' @param type Consumption type(s): "food", "feed", "processed", "other_util",
#' "bioenergy", "seed", "waste", "dom_balanceflow; NULL returns all types
#' @param type_aggr aggregate over demand types or not (boolean, default TRUE)
#' @return A MAgPIE object containing consumption value in million $US.
#' @author Miodrag Stevanovic
#' @examples
#'
#'   \dontrun{
#'     x <- consumptionValue(gdx)
#'   }
#' @importFrom luscale superAggregate

consumptionValue <- function(gdx, file = NULL, level = "reg", products = "kall",
                             product_aggr = TRUE, type = NULL, type_aggr = TRUE) {

  demand <- demand(gdx, level = "reg", products = products, product_aggr = FALSE,
                   attributes = "dm", type = type, type_aggr = FALSE)
  prices <- prices(gdx, level = "reg", products = products, product_aggr = FALSE,
                   attributes = "dm", type = "consumer")

  out <- demand * prices

  if (level != "reg") out <- superAggregateX(out, aggr_type = "sum", level = level)
  if (product_aggr)   out <- dimSums(out, dim = 3.2)
  if (!is.null(type)) out <- out[, , type]
  if (type_aggr)      out <- dimSums(out, dim = "demand")

  out(out, file)
}