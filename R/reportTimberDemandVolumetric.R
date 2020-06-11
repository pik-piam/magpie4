#' @title reportTimberDemandVolumetric
#' @description reports MAgPIE demand for timber.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Timber demand
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportTimberDemandVolumetric(gdx)
#'   }
#' 

reportTimberDemandVolumetric<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    a <- TimberDemandVolumetric(gdx,level = "regglo")
    getNames(a) <- reportingnames(getNames(a))
    a <- mbind(setNames(dimSums(a,dim=3),"Roundwood"),a)
    getNames(a) <- paste0("Timber demand volumetric|",getNames(a))
    getNames(a) <- paste0(getNames(a)," (mio m3/yr)")
  } else {cat("Disabled for magpie run without dynamic forestry. ")}
  
  return(a)
}