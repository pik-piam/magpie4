#' @title reportCropareaGrid
#' @description reports croparea
#' 
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#' 
#' @param gdx GDX file
#' @param spamfiledirectory path of directory with disaggregation data
#' @return Croparea as MAgPIE object (million ha/yr)
#' @author Benjamin Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCropareaGrid(gdx)
#'   }
#' @importFrom magclass withMetadata
#' @importFrom magclass getMetadata<-

reportCropareaGrid <- function(gdx,spamfiledirectory=FALSE) {

  a <- croparea(gdx,level="grid",products="kcr",product_aggr=FALSE, water_aggr=TRUE)
  
  #aggreate and rename
  #x <- NULL
  #x <- mbind(x,setNames(dimSums(a,dim=3),"Resources|Land Cover|Cropland (million ha)"))
  #x <- mbind(x,setNames(a,paste0("Resources|Land Cover|Cropland|+|", reportingnames(getNames(a,dim=1))," (million ha)")))
  
  # no renaming for grid
  x=setNames(a, reportingnames(getNames(a,dim=1)))
  
  withMetadata(TRUE)
  getMetadata(x,type="unit")<-"million ha/yr"
  withMetadata(FALSE)
  
  return(x)
}

