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
    getNames(a) <- paste0("Resources|Timber operations|Area Newly Established|",getNames(a))
    getNames(a) <- paste0(getNames(a)," (mha per yr)")
  } else {cat("Disabled for magpie run without timber production.")}
  
  return(a)
}