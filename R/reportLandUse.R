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
  a <- land(gdx,level = "reg",types = NULL,subcategories = c("crop","forestry"),sum = FALSE)
  a <- setNames(a,gsub("indc","ndc",getNames(a)))

  #add global
  a <- mbind(a, setItems(dimSums(a,dim=1), dim = 1, "GLO"))

  #aggreate and rename
  x <- NULL
  x <- mbind(x,setNames(dimSums(a,dim=3),"Resources|Land Cover (million ha)"))
  x <- mbind(x,setNames(dimSums(a[,,c("crop_area","crop_fallow","crop_treecover")],dim=3),paste0("Resources|Land Cover|+|Cropland"," (million ha)")))
  x <- mbind(x,setNames(a[,,"crop_area"],paste0("Resources|Land Cover|Cropland|+|", reportingnames("crop_area")," (million ha)")))
  x <- mbind(x,setNames(a[,,"crop_fallow"],paste0("Resources|Land Cover|Cropland|+|", reportingnames("crop_fallow")," (million ha)")))
  x <- mbind(x,setNames(a[,,"crop_treecover"],paste0("Resources|Land Cover|Cropland|+|", reportingnames("crop_treecover")," (million ha)")))
  x <- mbind(x,setNames(a[,,"past"],paste0("Resources|Land Cover|+|", reportingnames("past")," (million ha)")))
  x <- mbind(x,setNames(a[,,"urban"],paste0("Resources|Land Cover|+|", reportingnames("urban")," (million ha)")))
  x <- mbind(x,setNames(a[,,"other"],paste0("Resources|Land Cover|+|", reportingnames("other")," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,c("primforest","secdforest","forestry_aff","forestry_ndc","forestry_plant")],dim=3),paste0("Resources|Land Cover|+|", reportingnames("forest")," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,c("primforest","secdforest")],dim=3),    paste0("Resources|Land Cover|Forest|+|", reportingnames("natrforest")," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,"primforest"],dim=3),    paste0("Resources|Land Cover|Forest|Natural Forest|+|", reportingnames("primforest")," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,"secdforest"],dim=3),  paste0("Resources|Land Cover|Forest|Natural Forest|+|", reportingnames("secdforest")," (million ha)")))
  x <- mbind(x,setNames(dimSums(a[,,c("forestry_aff","forestry_ndc","forestry_plant")],dim=3),            paste0("Resources|Land Cover|Forest|+|", reportingnames("forestry")," (million ha)")))
  s32_aff_plantation <- readGDX(gdx,"s32_aff_plantation")
  if(s32_aff_plantation == 0) {
    x <- mbind(x,setNames(dimSums(a[,,"forestry_plant"],dim=3),"Resources|Land Cover|Forest|Planted Forest|+|Plantations (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry_plant"],dim=3),"Resources|Land Cover|Forest|Planted Forest|Plantations|+|Timber (million ha)"))
    x <- mbind(x,setNames(new.magpie(getRegions(a), getYears(a), NULL, fill = 0,sets = getSets(a)),"Resources|Land Cover|Forest|Planted Forest|Plantations|+|CO2-price AR (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,c("forestry_aff","forestry_ndc")],dim=3),"Resources|Land Cover|Forest|Planted Forest|+|Natural (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry_aff"],dim=3),"Resources|Land Cover|Forest|Planted Forest|Natural|+|CO2-price AR (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry_ndc"],dim=3),"Resources|Land Cover|Forest|Planted Forest|Natural|+|NPI_NDC AR (million ha)"))
  } else if (s32_aff_plantation == 1) {
    x <- mbind(x,setNames(dimSums(a[,,c("forestry_plant","forestry_aff")],dim=3),"Resources|Land Cover|Forest|Planted Forest|+|Plantations (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry_plant"],dim=3),"Resources|Land Cover|Forest|Planted Forest|Plantations|+|Timber (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry_aff"],dim=3),"Resources|Land Cover|Forest|Planted Forest|Plantations|+|CO2-price AR (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry_ndc"],dim=3),"Resources|Land Cover|Forest|Planted Forest|+|Natural (million ha)"))
    x <- mbind(x,setNames(new.magpie(getRegions(a), getYears(a), NULL, fill = 0,sets = getSets(a)),"Resources|Land Cover|Forest|Planted Forest|Natural|+|CO2-price AR (million ha)"))
    x <- mbind(x,setNames(dimSums(a[,,"forestry_ndc"],dim=3),"Resources|Land Cover|Forest|Planted Forest|Natural|+|NPI_NDC AR (million ha)"))
  }
  x <- mbind(x,setNames(dimSums(a[,,c("crop_area","crop_fallow","crop_treecover","past")],dim=3),"Resources|Land Cover|Agricultural land (million ha)"))

  return(x)
}

