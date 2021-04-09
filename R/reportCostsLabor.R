#' @title reportCostsLabor
#' @description reports MAgPIE labor costs
#' 
#' @param gdx GDX file
#' @return magpie object with labor costs
#' @author Debbora Leip
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCostsLabor(gdx)
#'   }
#'   
#' @importFrom magpiesets reporthelper summationhelper
#' 
reportCostsLabor <- function(gdx) {
  labor_costs <- CostsLabor(gdx, level = "regglo")
  
  labor_costs <- reporthelper(labor_costs, dim = 3.1, level_zero_name = "Costs|Labor", detail = TRUE)
  labor_costs <- summationhelper(labor_costs)
  
  getNames(labor_costs) <- paste0(getNames(labor_costs)," (million US$05/yr)")
  
  return(labor_costs)
}
