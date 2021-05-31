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
#' @importFrom magpiesets reporthelper
#'
reportCostTC <- function(gdx) {
  tcCost <- CostTC(gdx, level = "regglo")
  getNames(tcCost) <- "Costs|TC (million US$05/yr)"
  return(tcCost)
}
