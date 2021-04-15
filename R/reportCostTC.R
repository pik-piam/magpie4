#' @title reportCostTC
#' @description reports MAgPIE TC costs
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
#' @importFrom magpiesets reporthelper summationhelper
#' 
reportCostTC <- function(gdx) {
  tc_cost <- CostTC(gdx, level = "regglo")
  
  tc_cost <- reporthelper(tc_cost, dim = 3.1, level_zero_name = "Costs|TC", detail = TRUE)
  getNames(tc_cost) <- paste0(getNames(tc_cost)," (million US$05/yr)")
  
  return(tc_cost)
}
