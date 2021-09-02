#' @title reportProtectedArea
#' @description reports protected areas
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return protected area in Mha
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportProtectedArea(gdx)
#'   }
#' 

reportProtectedArea <- function(gdx) {
  
  #read in regional data
  a <- protectedArea(gdx,level = "reg")
  
  #add global
  a <- mbind(a, setItems(dimSums(a,dim=1), dim = 1, "GLO"))
  
  #aggreate and rename
  x <- NULL
  x <- mbind(x,setNames(a[,,"primforest"],paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("primforest"),"|Protected (million ha)")))
  x <- mbind(x,setNames(a[,,"secdforest"],paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("secdforest"),"|Protected (million ha)")))
  x <- mbind(x,setNames(a[,,"other"],paste0("Resources|Land Cover|", reportingnames(getNames(a[,,"other"],dim=1)),"|Protected (million ha)")))
  
  return(x)
}

