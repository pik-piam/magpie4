#' @title PlantationEstablishment
#' @description reads carbon stocks in harvested timber out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Area newly established in current time step for future timber production
#' @return Area newly for timber production
#' @author Abhijeet Mishra
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- PlantationEstablishment(gdx)
#'   }

PlantationEstablishment <- function(gdx, file=NULL, level="cell"){
  
  v32_land <- collapseNames(readGDX(gdx,"ov32_land",select = list(type="level"))[,,"plant"][,,"ac0"])
  
  a <- setNames(v32_land,"Forestry")
  a[,1,] = a[,1,]*5
  
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  
  out(a,file)
}