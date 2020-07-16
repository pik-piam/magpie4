#' @title reportPeatland
#' @description reports peatland area
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return peatland area as magclass object (million ha)
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPeatland(gdx)
#'   }
#' 

reportPeatland <- function(gdx) {
  
  x <- NULL
  
  a <- PeatlandArea(gdx,level="regglo")
  if(!is.null(a)) {
    x <- mbind(x,setNames(dimSums(a,dim=3),"Resources|Peatland (million ha)"))
    x <- mbind(x,setNames(a[,,"intact"],paste0("Resources|Peatland|+|", reportingnames(getNames(a[,,"intact"],dim=1))," (million ha)")))
    x <- mbind(x,setNames(a[,,"degrad"],paste0("Resources|Peatland|+|", reportingnames(getNames(a[,,"degrad"],dim=1))," (million ha)"))) 
    x <- mbind(x,setNames(a[,,"rewet"],paste0("Resources|Peatland|+|", reportingnames(getNames(a[,,"rewet"],dim=1))," (million ha)")))
  }
  
  return(x)
}

