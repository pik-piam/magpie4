#' @title GrowingStock
#' @description reads woody growing stock out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param indicator If the reported numbers are relative (mio m3/ha) or absolute (mio. m3). Default is relative.
#' @details Growing stock for producing woody materials consist of growing stock from plantations (forestry), secondary and primary forest as well as other land (natveg)
#' @return Growing stock in m3 per ha
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

GrowingStock <- function(gdx, file=NULL, level="regglo",indicator="relative"){
  
  if(level=="regglo"){
    
    ac_sub <- readGDX(gdx,"ac_sub")
    
    wood_density <- 0.6 ## tDM/m3
    ## Multiple sources for this number 
    ## Check Table 2.8.1 in 2013 Revised Supplementary Methods and Good Practice Guidance Arising from the Kyoto Protocol
    
    pm_timber_yield <- readGDX(gdx,"pm_timber_yield")
    
    third_dim_length <- length(unlist(strsplit(names(dimnames(pm_timber_yield))[[3]],split = ".",fixed=T)))
    ###################
    ##### FORESTRY ####
    ###################
    
    ## mio. ha
    land_forestry <- collapseNames(readGDX(gdx,"ov32_land","ov_land_fore",select = list(type="level"))[,,"plant"][,,ac_sub]) 
    
    ## tDM per ha
    if(third_dim_length==2){
      standing_stock_forestry <- collapseNames(readGDX(gdx,"pm_timber_yield")[,,"forestry"][,,ac_sub]) 
    } else {
      standing_stock_forestry <- collapseNames(readGDX(gdx,"pm_timber_yield")[,,"forestry"][,,"plantations"][,,ac_sub])
    }
    
    standing_volume_forestry <-   land_forestry * standing_stock_forestry / wood_density   ### mio. ha * tDM per ha / tDM per m3 = mio. m3
    
    ## Aggregate to level  --- Sum over dim 3 because we don't care about age-class differentiation at this point 
    standing_volume_forestry <- superAggregate(dimSums(standing_volume_forestry,dim=3), aggr_type = "sum", level = level,na.rm = FALSE)
    
    if(indicator == "relative"){
      gs_forestry <- standing_volume_forestry/superAggregate(dimSums(land_forestry,dim=3), aggr_type = "sum", level = level,na.rm = FALSE)
    } else {
      gs_forestry <- standing_volume_forestry
    }
    
    gs_forestry <- setNames(gs_forestry,"forestry")    ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
    
    ####################################################################################################
    
    #####################
    ##### SECDFOREST ####
    #####################
    
    ## mio. ha
    land_secdforest <- collapseNames(readGDX(gdx,"ov35_secdforest",select = list(type="level"))[,,ac_sub]) 
    
    ## tDM per ha
    if(third_dim_length==2){
      standing_stock_secdforest <- collapseNames(readGDX(gdx,"pm_timber_yield")[,,"secdforest"][,,ac_sub])
    } else{
      standing_stock_secdforest <- collapseNames(readGDX(gdx,"pm_timber_yield")[,,"secdforest"][,,"natveg"][,,ac_sub]) 
    }
    
    standing_volume_secdforest <-   land_secdforest * standing_stock_secdforest / wood_density   ### mio. ha * tDM per ha / tDM per m3 = mio. m3
    
    ## Aggregate to level  --- Sum over dim 3 because we don't care about age-class differentiation at this point 
    standing_volume_secdforest <- superAggregate(dimSums(standing_volume_secdforest,dim=3), aggr_type = "sum", level = level,na.rm = FALSE)
    
    if(indicator == "relative"){
      gs_secdforest <- standing_volume_secdforest/superAggregate(dimSums(land_secdforest,dim=3), aggr_type = "sum", level = level,na.rm = FALSE)
    } else {
      gs_secdforest <- standing_volume_secdforest
    }
    
    gs_secdforest <- setNames(gs_secdforest,"secdforest")    ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
    
    ####################################################################################################
    
    #####################
    ##### PRIMFOREST ####
    #####################
    
    ## mio. ha
    land_primforest <- collapseNames(readGDX(gdx,"ov_land",select = list(type="level"))[,,"primforest"]) 
    
    ## tDM per ha
    if(third_dim_length==2){
      standing_stock_primforest <- collapseNames(readGDX(gdx,"pm_timber_yield")[,,"primforest"][,,"acx"])
    } else{
      standing_stock_primforest <- collapseNames(readGDX(gdx,"pm_timber_yield")[,,"primforest"][,,"natveg"][,,"acx"])
    }
    
    standing_volume_primforest <-   land_primforest * standing_stock_primforest / wood_density   ### mio. ha * tDM per ha / tDM per m3 = mio. m3
    
    ## Aggregate to level  --- Sum over dim 3 because we don't care about age-class differentiation at this point 
    standing_volume_primforest <- superAggregate(dimSums(standing_volume_primforest,dim=3), aggr_type = "sum", level = level,na.rm = FALSE)
    
    if(indicator == "relative"){
      gs_primforest <- standing_volume_primforest/superAggregate(dimSums(land_primforest,dim=3), aggr_type = "sum", level = level,na.rm = FALSE)
    } else {
      gs_primforest <- standing_volume_primforest
    }
    
    gs_primforest <- setNames(gs_primforest,"primforest")    ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
    
    ####################################################################################################
    
    ################
    ##### OTHER ####
    ################
    
    ## mio. ha
    land_other <- collapseNames(readGDX(gdx,"ov35_other",select = list(type="level"))[,,ac_sub]) 
    
    ## tDM per ha
    if(third_dim_length==2){
      standing_stock_other <- collapseNames(readGDX(gdx,"pm_timber_yield")[,,"other"][,,ac_sub])
    } else {
      standing_stock_other <- collapseNames(readGDX(gdx,"pm_timber_yield")[,,"other"][,,"natveg"][,,ac_sub])
    }
    
    standing_volume_other <-   land_other * standing_stock_other / wood_density   ### mio. ha * tDM per ha / tDM per m3 = mio. m3
    
    ## Aggregate to level  --- Sum over dim 3 because we don't care about age-class differentiation at this point 
    standing_volume_other <- superAggregate(dimSums(standing_volume_other,dim=3), aggr_type = "sum", level = level,na.rm = FALSE)
    
    if(indicator == "relative"){
      gs_other <- standing_volume_other/superAggregate(dimSums(land_other,dim=3), aggr_type = "sum", level = level,na.rm = FALSE)
    } else {
      gs_other <- standing_volume_other
    }

    gs_other <- setNames(gs_other,"other")    ## Summing over age classes in dim 3 while retaining cluster cells in dim 1
    
    
    ####################################################################################################
    
    standing_volume_total <- standing_volume_forestry + standing_volume_primforest + standing_volume_secdforest + standing_volume_other
    
    ## Combine all GS together
    if(indicator == "relative"){

      land_total <- superAggregate(dimSums(land_forestry,dim=3), aggr_type = "sum", level = level,na.rm = FALSE) + 
        superAggregate(dimSums(land_primforest,dim=3), aggr_type = "sum", level = level,na.rm = FALSE) + 
        superAggregate(dimSums(land_secdforest,dim=3), aggr_type = "sum", level = level,na.rm = FALSE) + 
        superAggregate(dimSums(land_other,dim=3), aggr_type = "sum", level = level,na.rm = FALSE)
    
      gs_total <- setNames(standing_volume_total/land_total,"forest")
      
      a <- round(mbind(gs_total,gs_forestry,gs_secdforest,gs_primforest,gs_other),digits = 2)
    } else if(indicator == "absolute") {
      a <- mbind(setNames(standing_volume_total,"forest"),
                 setNames(standing_volume_forestry,"forestry"),
                 setNames(standing_volume_secdforest,"secdforest"),
                 setNames(standing_volume_primforest,"primforest"),
                 setNames(standing_volume_other,"other"))
      } else {stop("Invalid indicator ",indicator)}
    
  } else { 
    message("ERROR - wrong regions")
    a <- NULL
    }

  out(a,file)
}
