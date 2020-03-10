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
  
  ac_sub <- readGDX(gdx,"ac_sub")
  
  ###################
  ##### FORESTRY ####
  ###################
  
  gs_plantations <- collapseNames(readGDX(gdx,"ov32_land",select = list(type="level"))[,,"plant"][,,ac_sub]) * readGDX(gdx,"pm_growing_stock")[,,"forestry"][,,ac_sub] ## Cluster cells in dim 1, age classes in dim 3
  gs_plantations <- setNames(dimSums(gs_plantations,dim=3),"forestry")     ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
  
  ####################################################################################################
  
  #####################
  ##### SECDFOREST ####
  #####################
  
  gs_secdforest <- readGDX(gdx,"ov35_secdforest",select = list(type="level"))[,,ac_sub] * readGDX(gdx,"pm_growing_stock")[,,"secdforest"][,,ac_sub] ## Cluster cells in dim 1, age classes in dim 3
  gs_secdforest <- setNames(dimSums(gs_secdforest,dim=3),"secdforest")       ## Summing over age classes in dim 3 while retaining cluster cells in dim 1

  ####################################################################################################
  
  #####################
  ##### PRIMFOREST ####
  #####################
  
  gs_primforest <- readGDX(gdx,"ov_land",select = list(type="level"))[,,"primforest"] * readGDX(gdx,"pm_growing_stock")[,,"primforest"][,,"acx"] ## Cluster cells in dim 1, age classes in dim 3
  gs_primforest <- setNames(dimSums(gs_primforest,dim=3),"primforest")       ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
  
  ####################################################################################################
  
  ################
  ##### OTHER ####
  ################
  
  gs_other <- readGDX(gdx,"ov35_other",select = list(type="level"))[,,ac_sub] * readGDX(gdx,"pm_growing_stock")[,,"other"][,,ac_sub] ## Cluster cells in dim 1, age classes in dim 3
  gs_other <- setNames(dimSums(gs_other,dim=3),"other")       ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
  
  
  ####################################################################################################
  gs_all <- setNames(dimSums(gs_plantations + gs_secdforest + gs_primforest + gs_other, dim=3),"forest")
  ## Combine all GS together
  gs_world <- round(mbind(gs_all,gs_plantations,gs_secdforest,gs_primforest,gs_other)/1000,digits = 3)
  
  #aggregate over regions
  if (level != "cell") gs_world <- superAggregate(gs_world, aggr_type = "sum", level = level,na.rm = FALSE)
  
  a <- gs_world

  out(a,file)
}
