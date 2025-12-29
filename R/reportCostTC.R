#' @title reportCostTC
#' @description reports MAgPIE TC costs
#'
#' @export
#'
#' @param gdx GDX file
#' @return magpie object with TC costs
#' @author David Chen
#' @examples
#' \dontrun{
#' x <- reportCostTC(gdx)
#' }
#'
#'
#' @section TC cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs\|TC | million US$2017/yr | Technological change investment costs
#' @md

#' @importFrom magpiesets reporthelper
#'
reportCostTC <- function(gdx) {
  tcCost <- CostTC(gdx, level = "regglo")
  getNames(tcCost) <- "Costs|TC (million US$2017/yr)"
  return(tcCost)
}
