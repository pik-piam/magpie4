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
#'
#' @section Consumption value variables:
#' Name | Unit | Meta
#' ---|---|---
#' Value\|Consumption Value | million US$2017/yr | Total value of agricultural consumption
#' @md

#' @importFrom magclass getNames

reportConsumVal <- function(gdx, level = "regglo") {

  # Consumption value calculation
  x <- consumptionValue(gdx, level = level)
  getNames(x) <- "Value|Consumption Value (million US$2017/yr)"

  return(x)
}
