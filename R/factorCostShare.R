#' @title factorCostShares
#' @description returns labor and capital cost share out of factor costs (i.e. labor + capital)
#'
#' @export
#'
#' @param gdx GDX file
#' @param products products for which cost shares should be reported, kcr or kli
#' @param level spatial aggregation to report employment ("reg", "glo" or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @return labor and capital cost share out of factor costs
#' @author Debbora Leip
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- factorCostShares(gdx)
#' }

factorCostShares <- function(gdx, products = "kli", level = "reg", file = NULL) {

  if (products == "kcr") {
    x <- readGDX(gdx, "p38_cost_share", react = "silent")
    w <- factorCosts(gdx, products = products, level = "reg")[, , "factor_costs", drop = TRUE]
  } else if (products == "kli") {
    x <- readGDX(gdx, "p70_cost_share_livst", react = "silent")
    w <- factorCosts(gdx, products = products, level = "reg")[, , "factor_costs", drop = TRUE]
  } else {
    stop("Product type not supported")
  }

  if (!is.null(x)) {
    w <- mbind(setNames(w, "labor"), setNames(w, "capital"))
    x <- superAggregate(x, aggr_type = "weighted_mean", weight = w, level = level)
  }


  out(x, file)
}
