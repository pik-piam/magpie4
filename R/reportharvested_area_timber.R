#' @title reportharvested_area_timber
#' @description reports MAgPIE harvested area for timber.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Area harvested for timber production
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportharvested_area_timber(gdx)
#'   }
#' 

reportharvested_area_timber<-function(gdx){
  a <- NULL
  
  timber <- FALSE
  fore_red <- readGDX(gdx, "ov_hvarea_forestry", "ov32_hvarea_forestry", "ov32_land_reduction", "ov_forestry_reduction", select = list(type = "level"), react = "silent", format = "first_found")
  if (!is.null(fore_red)) {
    if (max(fore_red) > 1) {
      if (readGDX(gdx, "s73_timber_demand_switch", "sm_timber_demand_switch")) {
        timber <- TRUE
      }
    }
  }
  
  if(timber){
    a <- harvested_area_timber(gdx,level = "regglo")
    a <- mbind(a,setNames(dimSums(a,dim=3),"Total"))
    getNames(a) <- paste0("Resources|Timber operations|Harvested area for timber|",getNames(a))
    getNames(a) <- paste0(getNames(a)," (mha per yr)")
  } else {cat("Disabled for magpie run without timber production.")}
  
  return(a)
}