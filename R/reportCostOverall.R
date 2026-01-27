#' @title reportCostOverall
#' @description reports MAgPIE costs
#'
#' @param gdx GDX file
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#'
#'   \dontrun{
#'     x <- reportCostOverall(gdx)
#'   }
#'
#' @section Overall cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs\|Gross value of production | million US$2017/yr | Total gross value of agricultural production
#' @md
#' @importFrom magclass getNames
#' @export
reportCostOverall <- function(gdx) {
  #Gross Value of production
  x <- CostOverall(gdx, level = "regglo")
  getNames(x) <- "Costs|Gross value of production"

  getNames(x) <- paste0(getNames(x), " (million US$2017/yr)")

  return(x)
}