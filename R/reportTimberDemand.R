#' @title reportTimberDemand
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
#'     x <- reportTimberDemand(gdx)
#'   }
#' 
#'
#' @section Timber demand variables:
#' Name | Unit | Meta
#' ---|---|---
#' Timber demand\|Roundwood | mio tDM | Total roundwood demand
#' Timber demand\|Industrial roundwood | mio tDM | Industrial wood demand
#' Timber demand\|Wood fuel | mio tDM | Wood fuel demand
#' @md


reportTimberDemand<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    a <- TimberDemand(gdx,level = "regglo")
    a <- mbind(setNames(dimSums(a,dim=3),"Roundwood"),a)
    getNames(a) <- paste0("Timber demand|",getNames(a))
    getNames(a) <- paste0(getNames(a)," (mio tDM)")
  } else {message("Disabled (no timber) ", appendLF = FALSE)}
  
  return(a)
}