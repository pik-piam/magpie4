#' @title reportCostTC
#' @description reports MAgPIE TC costs
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return magpie object with TC costs
#' @author David Chen
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCostTC(gdx)
#'   }
#'   
#' @importFrom magpiesets reporthelper 
#' 
reportCostTC <- function(gdx) {
  tc_cost <- CostTC(gdx, level = "regglo")
  getNames(tc_cost) <- "Costs|TC (million US$05/yr)"
  return(tc_cost)
}
