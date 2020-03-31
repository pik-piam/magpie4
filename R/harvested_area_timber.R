#' @title harvested_area_timber
#' @description reads carbon stocks in harvested timber out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Area harvested for timber production
#' @return Area harvested for timber production
#' @author Abhijeet Mishra
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- harvested_area_timber(gdx)
#'   }

harvested_area_timber <- function(gdx, file=NULL, level="cell"){
  
  ac_sub <- readGDX(gdx,"ac_sub")
  
  ov_hvarea_forestry <- readGDX(gdx,"ov_hvarea_forestry",select = list(type="level"))[,,ac_sub]
  vm_hvarea_secdforest <- readGDX(gdx,"ov_hvarea_secdforest",select = list(type="level"))[,,ac_sub]
  vm_hvarea_primforest <- readGDX(gdx,"ov_hvarea_primforest",select = list(type="level"))
  vm_hvarea_other <- readGDX(gdx,"ov73_hvarea_other",select = list(type="level"))[,,ac_sub]

  a <- mbind(setNames(dimSums(ov_hvarea_forestry,dim=3),"Forestry"), 
             setNames(dimSums(vm_hvarea_secdforest,dim=3),"Secondary forest"),
             setNames(dimSums(vm_hvarea_primforest,dim=3),"Primary forest"),
             setNames(dimSums(vm_hvarea_other,dim=3),"Other land"))

  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  
  out(a,file)
}