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

reportPlantationEstablishment<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    a <- PlantationEstablishment(gdx,level = "regglo")
    getNames(a) <- paste0("Area Newly Established|",getNames(a))
    getNames(a) <- paste0(getNames(a)," (million ha)")
  } else {cat("NULL returned for magpie run without dynamic forestry.")}
  
  return(a)
}