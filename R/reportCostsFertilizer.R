#' @title reportCostsFertilizer
#' @description reports MAgPIE nitrogen fertilizer costs disaggregated to crop categories
#' 
#' @param gdx GDX file
#' @return magpie object with fertilizer costs
#' @author Debbora Leip
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCostsFertilizer(gdx)
#'   }
#'   
#' @importFrom magpiesets reporthelper summationhelper
#'    
reportCostsFertilizer <- function(gdx){
  nr_fertilizer_costs <- CostsFertilizer(gdx, level = "regglo")
  # no phosphorus fertililzer costs in MAgPIE
  
  nr_fertilizer_costs <- reporthelper(nr_fertilizer_costs, dim = 3.1, level_zero_name = "Costs|Fertiliizer", detail = TRUE)
  nr_fertilizer_costs <- summationhelper(nr_fertilizer_costs)
  
  getNames(nr_fertilizer_costs) <- paste0(getNames(nr_fertilizer_costs), " (million US$05/yr)")
  
  return(nr_fertilizer_costs)
}