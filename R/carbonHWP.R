#' @title carbonHWP
#' @description reads carbon stocks in harvested timber out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details carbon pools consist of vegetation carbon (vegc), litter carbon (litc) and soil carbon (soilc)
#' @return carbon stocks in MtC from harvested timber
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- carbonHWP(gdx)
#'   }

carbonHWP <- function(gdx, file=NULL, level="cell"){
  ov32_hvarea_forestry <- readGDX(gdx,"ov32_hvarea_forestry",select = list(type="level"))
  p32_carbon_density_ac <- collapseNames(dimSums(readGDX(gdx,"p32_carbon_density_ac"),dim=3.2)[,,"plant"][,,"vegc"])
  hwp_forestry <- ov32_hvarea_forestry*p32_carbon_density_ac
  hwp_forestry <- dimSums(hwp_forestry,dim=3.2)
  
  ov35_hvarea_secdforest <- readGDX(gdx,"ov35_hvarea_secdforest",select = list(type="level"))
  ov35_hvarea_primforest <- readGDX(gdx,"ov35_hvarea_primforest",select = list(type="level"))
  ov35_hvarea_other <- readGDX(gdx,"ov35_hvarea_other",select = list(type="level"))
  pm_carbon_density_ac <- collapseNames(dimSums(readGDX(gdx,"pm_carbon_density_ac"),dim=3.1)[,,"vegc"])
  
  hwp_secdforest <- ov35_hvarea_secdforest*pm_carbon_density_ac
  hwp_secdforest <- dimSums(hwp_secdforest,dim=3.2)
  hwp_primforest <- ov35_hvarea_primforest*pm_carbon_density_ac
#  hwp_primforest <- dimSums(hwp_primforest,dim=3.2)
  hwp_other <- ov35_hvarea_other*pm_carbon_density_ac
  hwp_other <- dimSums(hwp_other,dim=3.2)
  
  a <- hwp_forestry+hwp_secdforest+hwp_primforest+hwp_other
  
  #aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  
  out(a,file)
}