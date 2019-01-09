#' @title carbonHWP
#' @description reads carbon stocks in harvested timber out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param unit element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr
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

carbonHWP <- function(gdx, file=NULL, level="cell",unit="element"){

#   ac_sub <- intersect(getNames(ov32_hvarea_forestry,dim=2), getNames(p32_carbon_density_ac,dim=1))
#   
#   kg_bcef <- dimSums(readGDX(gdx,"pm_climate_class")*readGDX(gdx,"pm_bcef"),dim=3.1)
#   
#   ov32_hvarea_forestry <- readGDX(gdx,"ov32_hvarea_forestry",select = list(type="level"))
#   p32_carbon_density_ac <- collapseNames(readGDX(gdx,"p32_carbon_density_ac")[,,"plant"][,,"vegc"])
#   hwp_forestry <- ov32_hvarea_forestry[,,ac_sub] * p32_carbon_density_ac[,,ac_sub] / kg_bcef[,,ac_sub]
#   hwp_forestry <- dimSums(hwp_forestry,dim=3.2)
#   
#   ov35_hvarea_secdforest <- readGDX(gdx,"ov35_hvarea_secdforest",select = list(type="level"))
#   ov35_hvarea_primforest <- readGDX(gdx,"ov35_hvarea_primforest",select = list(type="level"))
#   ov35_hvarea_other <- readGDX(gdx,"ov35_hvarea_other",select = list(type="level"))
#   pm_carbon_density_ac <- collapseNames(readGDX(gdx,"pm_carbon_density_ac")[,,"vegc"])
#   
#   ## common ac_sub ####################################################### xxxxxxxxxxxx ######################
#   hwp_secdforest <- ov35_hvarea_secdforest[,,ac_sub]*pm_carbon_density_ac[,,ac_sub] / kg_bcef[,,ac_sub]
#   hwp_secdforest <- dimSums(hwp_secdforest,dim=3.2)
#   hwp_primforest <- collapseNames(ov35_hvarea_primforest*pm_carbon_density_ac[,,"acx"] / kg_bcef[,,"acx"])
# #  hwp_primforest <- dimSums(hwp_primforest,dim=3.2)
#   hwp_other <- ov35_hvarea_other[,,ac_sub]*pm_carbon_density_ac[,,ac_sub] / kg_bcef[,,ac_sub]
#   hwp_other <- dimSums(hwp_other,dim=3.2)
#   
#   a <- hwp_forestry+hwp_secdforest+hwp_primforest+hwp_other
  
  ## How much timber was produced?
  ## Demand equals production so we can read in the demand directly instead of production
  
  timber_production <- collapseNames(readGDX(gdx,"ov_prod_forestry")[,,"level"]+readGDX(gdx,"ov_prod_natveg")[,,"level"]) ## mio. m3
  
  ## We convert this harvested timber to mio.ton
  timber_mass <- timber_production * readGDX(gdx,"p16_volumetric_conversion") / 1000 ## This is now in mio.ton
  
  timestep_length <- readGDX(gdx,"im_years",react="silent")
  if(is.null(timestep_length)) timestep_length <- timePeriods(gdx)
  
  timber_mass <- timber_mass / timestep_length ## Per year
  ## Approximately 0.5kg of carbon in 1kg of wood
  c_stored <- timber_mass * 0.5
  
  if(unit=="gas") c_stored <- c_stored * 44 / 12
  
  #aggregate over regions
  if (level != "cell") c_stored <- superAggregate(c_stored, aggr_type = "sum", level = level,na.rm = FALSE)
  
  a <- c_stored
  
  out(a,file)
}