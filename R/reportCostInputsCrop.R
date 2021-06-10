#' @title reportCostInputsCrop
#' @description reports MAgPIE costs
#'
#' @export
#'
#' @param gdx GDX file
#'
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- reportCostInputsCrop(gdx)
#' }
#' @importFrom magclass getNames

reportCostInputsCrop <- function(gdx) {

  if (suppressWarnings(is.null(readGDX(gdx, "ov_cost_inv")))) {

    out <- CostInputFactorsCrop(gdx, type = NULL, level = "regglo")

  } else {

    out <- CostInputFactorsCrop(gdx, type = "overall", level = "regglo")

  }

  getNames(out) <- "Costs|Input Costs for crops (million US$05/yr)"
  return(out)
}
