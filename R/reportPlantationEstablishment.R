#' @title reportPlantationEstablishment
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
#'     x <- reportPlantationEstablishment(gdx)
#'   }
#' 
#'
#' @section Plantation establishment variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Timber operations\|Area Newly Established\|Forestry | mha per yr | Annual area of new timber plantations established
#' @md


reportPlantationEstablishment<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    a <- PlantationEstablishment(gdx,level = "regglo") 
    getNames(a) <- paste0("Resources|Timber operations|Area Newly Established|",getNames(a))
    getNames(a) <- paste0(getNames(a)," (mha per yr)")
  } else {message("Disabled (no timber) ", appendLF = FALSE)}
  
  return(a)
}