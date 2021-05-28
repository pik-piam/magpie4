#' @title reportTimberProductionVolumetric
#' @description reports MAgPIE production for timber.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Timber demand
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportTimberProductionVolumetric(gdx)
#'   }
#' 
#' @importFrom magpiesets reportingnames

reportTimberProductionVolumetric<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    a <- TimberProductionVolumetric(gdx,level = "regglo")
    getNames(a) <- reportingnames(getNames(a))
    a <- mbind(setNames(dimSums(a,dim=3),"Roundwood"),a)
    getNames(a) <- paste0("Timber production volumetric|",getNames(a))
    getNames(a) <- paste0(getNames(a)," (mio m3/yr)")
  } else {message("Disabled (no timber) ", appendLF = FALSE)}
  
  return(a)
}