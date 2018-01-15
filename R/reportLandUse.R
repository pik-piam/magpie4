#' @title reportLandUse
#' @description reports land-use
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return land-use as MAgPIE object (million ha)
#' @author Florian Humpenoeder, Kristine Karstens
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
  
  #add global
  a <- mbind(a,dimSums(a,dim=1))
  
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
  
  ### subcatergories for forest(ry) -> Should be moved to reportForestForestryArea (like reportCropArea)
  asub <- getNames(a,dim=2)
  if("old" %in% asub){
    x <- mbind(x,setNames(dimSums(a[,,"forestry"][,,"old"],dim=3),"Resources|Land Cover|Forest|Plantations|Forestry (million ha)"))
  } else if("plant" %in% asub){
    x <- mbind(x,setNames(dimSums(a[,,"forestry"][,,"plant"],dim=3),"Resources|Land Cover|Forest|Plantations|Forestry (million ha)"))
  }
  
  if(all(c("new","prot","grow") %in% asub)){
    x <- mbind(x,setNames(dimSums(a[,,"forestry"][,,c("new","prot","grow")],dim=3),"Resources|Land Cover|Forest|Plantations|Afforestation (million ha)"))  
  } else if(all(c("aff","indc") %in% asub)){
    x <- mbind(x,setNames(dimSums(a[,,"forestry"][,,c("aff","indc")],dim=3),"Resources|Land Cover|Forest|Plantations|Afforestation (million ha)"))
  }
    
  return(x)
}

