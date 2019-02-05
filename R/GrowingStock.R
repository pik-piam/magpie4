#' @title GrowingStock
#' @description reads woody growing stock out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Growing stock for producing woody materials consist of growing stock from plantations (forestry), secondary and primary forest as well as other land (natveg)
#' @return Growing stock in billion m3
#' @author Abhijeet Mishra
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- GrowingStock(gdx)
#'   }
#' 

GrowingStock <- function(gdx, file=NULL, level="cell"){
  
  ###################
  ##### FORESTRY ####
  ###################
  
  ## Read in yields of plantations which is already scaled up within the gams code
  p32_yield_forestry_ac <- readGDX(gdx,"p32_yield_forestry_ac")  ## Cluster cells in dim 1, age classes in dim 3
  ## Read the land available for plantations in each step
  ov32_land <- collapseNames(readGDX(gdx,"ov32_land",select = list(type="level"))[,,"plant"]) ## Cluster cells in dim 1, age classes in dim 3
  
  ## Now p32_yield_forestry_ac is in m3/ha and ov32_land is in mio. ha.
  ## If we multiply these, we get the GS in mio. m3 units and a further division by 1000 will give us GS in bio. m3 units
  
  gs_plantations <- ov32_land * p32_yield_forestry_ac ## Cluster cells in dim 1, age classes in dim 3
  gs_plantations <- setNames(dimSums(gs_plantations,dim=3),"forestry")     ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
  # names(dimnames(gs_plantations))[3] <- "source"
  # gs_plantations <- add_dimension(gs_plantations,dim = 3.1,add = "forest",nm = "Forest")
  
  ####################################################################################################
  
  #####################
  ##### SECDFOREST ####
  #####################
  
  ## Read in yields of secondary forests
  p35_yield_natveg <- readGDX(gdx,"p35_yield_natveg")  ## Cluster cells in dim 1, age classes in dim 3
  ## Read the land available for secondary forests in each step
  ov35_secdforest <- readGDX(gdx,"ov35_secdforest",select = list(type="level")) ## Cluster cells in dim 1, age classes in dim 3
  
  ## Now p35_yield_natveg is in m3/ha and ov35_secdforest is in mio. ha.
  ## If we multiply these, we get the GS in mio. m3 units and a further division by 1000 will give us GS in bio. m3 units
  
  gs_secdforest <- ov35_secdforest * p35_yield_natveg ## Cluster cells in dim 1, age classes in dim 3
  gs_secdforest <- setNames(dimSums(gs_secdforest,dim=3),"secdforest")       ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
  # names(dimnames(gs_secdforest))[3] <- "source"
  # gs_secdforest <- add_dimension(gs_secdforest,dim = 3.1,add = "forest",nm = "Forest")
  
  ####################################################################################################
  
  #####################
  ##### PRIMFOREST ####
  #####################
  
  ## Read in yields of primary forests 
  p35_yield_primforest <- readGDX(gdx,"p35_yield_primforest")  ## Cluster cells in dim 1, No age classes in dim 3 as prim forests are of only age class acx
  ## Read the land available for primary forests in each step
  ov_land_primforest <- collapseNames(readGDX(gdx,"ov_land",select = list(type="level"))[,,"primforest"]) ## Cluster cells in dim 1, Only acx in dim 3
  
  ## Now p35_yield_primforest is in m3/ha and ov_land_primforest is in mio. ha.
  ## If we multiply these, we get the GS in mio. m3 units and a further division by 1000 will give us GS in bio. m3 units
  
  gs_primforest <- setNames(ov_land_primforest * p35_yield_primforest,"primforest") ## Cluster cells in dim 1, Only highest age class i.e acx in dim 3
  # names(dimnames(gs_primforest))[3] <- "source"
  # gs_primforest <- add_dimension(gs_primforest,dim = 3.1,add = "forest",nm = "Forest")
  
  ####################################################################################################
  
  ################
  ##### OTHER ####
  ################
  
  ## Read in yields of other land (which is same as the one used for secondary forests)
  p35_yield_other <- readGDX(gdx,"p35_yield_natveg")  ## Cluster cells in dim 1, age classes in dim 3
  ## Read the land available for other land in each step
  ov35_other <- readGDX(gdx,"ov35_other",select = list(type="level")) ## Cluster cells in dim 1, age classes in dim 3
  
  ## Now p35_yield_other is in m3/ha and ov35_other is in mio. ha.
  ## If we multiply these, we get the GS in mio. m3 units and a further division by 1000 will give us GS in bio. m3 units
  
  gs_other <- ov35_other * p35_yield_other  ## Cluster cells in dim 1, age classes in dim 3
  gs_other <- setNames(dimSums(gs_other,dim=3),"other")     ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
  # names(dimnames(gs_other))[3] <- "source"
  # gs_other <- add_dimension(gs_other,dim = 3.1,add = "forest",nm = "Forest")
  
  ####################################################################################################
  gs_all <- setNames(dimSums(gs_plantations + gs_secdforest + gs_primforest + gs_other, dim=3),"forest")
  ## Combine all GS together
  gs_world <- round(mbind(gs_all,gs_plantations,gs_secdforest,gs_primforest,gs_other)/1000,digits = 3)
  
  #aggregate over regions
  if (level != "cell") gs_world <- superAggregate(gs_world, aggr_type = "sum", level = level,na.rm = FALSE)
  
  a <- gs_world
  
  out(a,file)
}
