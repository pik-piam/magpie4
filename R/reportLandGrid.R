#' @title reportLandGrid
#' @description reports land-use on  a grid
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param spamfiledirectory path of directory wit disaggegation data
#' @return land-use as MAgPIE object (million ha)
#' @author Benjamin Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportLandGrid(gdx)
#'   }
#' @importFrom magclass withMetadata
#' @importFrom magclass getMetadata<-

reportLandGrid <- function(gdx,spamfiledirectory="") {
  
  ### main land types
  #read in regional data
  
  a <- land(gdx,level = "grid",types = NULL,sum = FALSE,spamfiledirectory=spamfiledirectory)

  #aggreate and rename
  #x <- NULL
  #x <- mbind(x,setNames(dimSums(a,dim=3),"Resources|Land Cover (million ha)"))
  getNames(a,dim=1) <- reportingnames(getNames(a,dim=1))
  #x <- mbind(x,setNames(a,paste0("Resources|Land Cover|+|", reportingnames(getNames(a,dim=1))," (million ha)")))

  withMetadata(TRUE)
  getMetadata(a,type="unit")<-"million ha/yr"
  withMetadata(FALSE)
  return(a)
  
}

