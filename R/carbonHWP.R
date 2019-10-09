#' @title carbonHWP
#' @description reads carbon stocks in harvested timber out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param unit element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr
#' @details Carbon stored in harvested wood products
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
  
  
  ov32_hvarea_forestry <- readGDX(gdx,"ov32_hvarea_forestry",select = list(type="level"))
  ac_sub <- readGDX(gdx,"ac_sub")

  pm_carbon_density_ac <- collapseNames(readGDX(gdx,"pm_carbon_density_ac")[,,"vegc"])
  p32_management_factor <- collapseNames(readGDX(gdx,"p32_management_factor"))
  if (length(getNames(collapseNames(readGDX(gdx,"p32_management_factor")))) > 0) {
    p32_carbon_density_ac <- pm_carbon_density_ac * p32_management_factor
    hwp_forestry <- ov32_hvarea_forestry[,,ac_sub] * p32_carbon_density_ac[,,ac_sub]
    hwp_forestry <- dimSums(hwp_forestry,dim=c(3.2,3.3))
    hwp_forestry <- add_dimension(hwp_forestry,dim = 3.1,add = "source",nm = "forestry")
  } else {
    p32_carbon_density_ac = pm_carbon_density_ac * p32_management_factor
    hwp_forestry <- ov32_hvarea_forestry[,,ac_sub] * p32_carbon_density_ac[,,ac_sub]
    hwp_forestry <- dimSums(hwp_forestry,dim=c(3.2))
    hwp_forestry <- add_dimension(hwp_forestry,dim = 3.1,add = "source",nm = "forestry")
    }

  ov35_hvarea_secdforest <- readGDX(gdx,"ov35_hvarea_secdforest",select = list(type="level"))
  ov35_hvarea_primforest <- readGDX(gdx,"ov35_hvarea_primforest",select = list(type="level"))
  ov35_hvarea_other <- readGDX(gdx,"ov35_hvarea_other",select = list(type="level"))

  ## common ac_sub ####################################################### xxxxxxxxxxxx ######################
  hwp_secdforest <- ov35_hvarea_secdforest[,,ac_sub]*pm_carbon_density_ac[,,ac_sub]
  hwp_secdforest <- dimSums(hwp_secdforest,dim=3.2)
  hwp_secdforest <- add_dimension(hwp_secdforest,dim = 3.1,add = "source",nm = "secdforest")

  hwp_primforest <- collapseNames(ov35_hvarea_primforest*pm_carbon_density_ac[,,"acx"])
#  hwp_primforest <- dimSums(hwp_primforest,dim=3.2)
  hwp_primforest <- add_dimension(hwp_primforest,dim = 3.1,add = "source",nm = "primforest")

  hwp_other <- ov35_hvarea_other[,,ac_sub]*pm_carbon_density_ac[,,ac_sub]
  hwp_other <- dimSums(hwp_other,dim=3.2)
  hwp_other <- add_dimension(hwp_other,dim = 3.1,add = "source",nm = "other")

  a <- dimSums(mbind(hwp_forestry,hwp_secdforest,hwp_primforest,hwp_other),dim=3.1)

   # all_prod <- readGDX(gdx,"ov_prod",select = list(type="level"))
   # timber <- all_prod[,,c("wood","woodfuel")]
   # 
   # # This is in mio. m3 - convert to mio. ton --- 1m3 wood fuel = 307 and 1m3 wood = 632kg
   # timber[,,"woodfuel"] <- timber[,,"woodfuel"] * 307 / 1000
   # timber[,,"wood"] <- timber[,,"wood"] * 632 / 1000
   # 
   # c_timber <- timber*0.5
   # 
   # a <- c_timber

  if(unit=="gas") a <- a * 44 / 12
  
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  
  out(a,file)
}