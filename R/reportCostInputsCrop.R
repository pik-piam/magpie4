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

  if (suppressWarnings(is.null(readGDX(gdx, "p38_capital_mobile")))) {

    out <- costInputFactorsCrop(gdx, type = NULL, level = "regglo")


  } else {

    out <- costInputFactorsCrop(gdx, type = "investment", level = "regglo")
    out <- dimSums(out, dim = 3.1)


  }

   getNames(out) <- "Costs|Inputs for crops (million US$2017/yr)"


  return(out)
}
