#' @title reportLandUse
#' @description reports land-use
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return land-use as MAgPIE object (million ha)
#' @author Florian Humpenoeder, Kristine Karstens, Isabelle Weindl
#' @examples
#' 
#'   \dontrun{
#'     x <- reportLandUse(gdx)
#'   }
#' 

reportLandUse <- function(gdx) {
  
  ### main land types
  #read in regional data
  a <- land(gdx,level = "reg",types = NULL,subcategories = c("forestry"),sum = FALSE)
  a <- setNames(a,gsub("indc","ndc",getNames(a)))
  
  #add global
  a <- mbind(a, setItems(dimSums(a,dim=1), dim = 1, "GLO"))
  
  #aggreate and rename
  x <- NULL
  x <- mbind(x,setNames(dimSums(a,dim=3),"Resources|Land Cover (million ha)"))
  x <- mbind(x,setNames(a[,,"crop"],paste0("Resources|Land Cover|+|", reportingnames(getNames(a[,,"crop"],dim=1))," (million ha)")))
  x <- mbind(x,setNames(a[,,"past"],paste0("Resources|Land Cover|+|", reportingnames(getNames(a[,,"past"],dim=1))," (million ha)"))) 
  x <- mbind(x,setNames(a[,,"urban"],paste0("Resources|Land Cover|+|", reportingnames(getNames(a[,,"urban"],dim=1))," (million ha)")))
  x <- mbind(x,setNames(a[,,"other"],paste0("Resources|Land Cover|+|", reportingnames(getNames(a[,,"other"],dim=1))," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,c("primforest","secdforest","forestry")],dim=3),paste0("Resources|Land Cover|+|", reportingnames("forest")," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,c("primforest","secdforest")],dim=3),    paste0("Resources|Land Cover|Forest|+|", reportingnames("natrforest")," (million ha)"))) 
  x <- mbind(x,setNames(dimSums(a[,,"primforest"],dim=3),    paste0("Resources|Land Cover|Forest|Natural Forest|+|", reportingnames("primforest")," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,"secdforest"],dim=3),  paste0("Resources|Land Cover|Forest|Natural Forest|+|", reportingnames("secdforest")," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,"forestry"],dim=3),            paste0("Resources|Land Cover|Forest|+|", reportingnames("forestry")," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,c("crop","past")],dim=3),"Resources|Land Cover|Agricultural land (million ha)"))
  
  ### subcatergories for land type forestry
  asub <- getNames(a,dim=2)
  #MAgPIE 4.0/4.1
  if(all(c("new","prot","grow","old") %in% asub)){
    x <- mbind(x,setNames(dimSums(a[,,"forestry"][,,"old"],dim=3),"Resources|Land Cover|Forest|Managed Forest|+|Plantations (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry"][,,"old",invert=T],dim=3),"Resources|Land Cover|Forest|Managed Forest|+|Afforestation (million ha)"))
  #MAgPIE 4.2
  } else if(all(c("aff","ndc","plant") %in% asub)){
    x <- mbind(x,setNames(dimSums(a[,,"forestry"][,,"plant"],dim=3),"Resources|Land Cover|Forest|Managed Forest|+|Plantations (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry"][,,"ndc"],dim=3),"Resources|Land Cover|Forest|Managed Forest|+|NPI/NDC (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry"][,,"aff"],dim=3),"Resources|Land Cover|Forest|Managed Forest|+|Afforestation (million ha)"))
  }

  return(x)
}

