#' @title reportConsumVal
#' @description reports MAgPIE consumption value
#'
#' @export
#'
#' @param gdx GDX file
#' @return Magpie object associated with the consumption value
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- reportConsumVal(gdx)
#' }
#' @importFrom magclass getNames

reportConsumVal <- function(gdx) {

  # Consumption value calculation
  x <- consumptionValue(gdx, level = "regglo")

  getNames(x) <- "Value|Consumption Value (million US$2017/yr)"


  return(x)


}
