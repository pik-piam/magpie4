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
#'
#' @section Timber harvested area variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Timber operations\|Harvested area for timber\|Forestry | mha per yr | Area harvested from managed forests
#' Resources\|Timber operations\|Harvested area for timber\|Primary forest | mha per yr | Area harvested from primary forests
#' Resources\|Timber operations\|Harvested area for timber\|Secondary forest | mha per yr | Area harvested from secondary forests
#' @md


reportharvested_area_timber<-function(gdx){
  a <- NULL

  timber <- FALSE
  if (as.numeric(readGDX(gdx, "s32_hvarea")) > 0 & as.numeric(readGDX(gdx, "s35_hvarea")) > 0) timber <- TRUE

  if(timber){
    a <- harvested_area_timber(gdx,level = "regglo")
    getNames(a) <- paste0("Resources|Timber operations|Harvested area for timber|",getNames(a))
    getNames(a) <- paste0(getNames(a)," (mha per yr)")
  } else {message("Disabled (no timber) ", appendLF = FALSE)}

  return(a)
}
