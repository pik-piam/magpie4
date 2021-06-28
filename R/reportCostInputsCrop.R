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

  if (suppressWarnings(is.null(readGDX(gdx, "p38_capital_mobile_t")))) {

    out <- costInputFactorsCrop(gdx, type = NULL, level = "regglo")

  } else {

    out <- costInputFactorsCrop(gdx, type = "overall", level = "regglo")

  }

  getNames(out) <- "Costs|Input Costs for crops (million US$05/yr)"
  return(out)
}
