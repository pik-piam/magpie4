#' @title ForestYield
#' @description reads timber yield out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param yield_type Establishment yields or harvest yields. Choose "harvest" or "establishment".
#' @details Forest yield for timber production
#' @return Forest yield for timber production
#' @author Abhijeet Mishra
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- ForestYield(gdx)
#'   }

ForestYield <- function(gdx, file=NULL, level="cell", yield_type = "harvest"){
  a <- NULL
  
  if(yield_type == "harvest"){
    if (level == "cell"){
      #### Production and harvest area calculations
      ov32_prod_forestry <- dimSums(readGDX(gdx,"ov32_prod",select = list(type="level")),dim=3)
      ov32_hvarea_forestry <- dimSums(readGDX(gdx,"ov32_hvarea_forestry",select = list(type="level")),dim=3)
      
      ov35_prod_secdf <- dimSums(readGDX(gdx,"ov35_prod",select = list(type="level"))[,,"secdforest"],dim=3)
      ov35_hvarea_secdf <- dimSums(readGDX(gdx,"ov35_hvarea_secdforest",select = list(type="level")),dim=3)
      
      ov35_prod_primf <- dimSums(readGDX(gdx,"ov35_prod",select = list(type="level"))[,,"primforest"],dim=3)
      ov35_hvarea_primf <- dimSums(readGDX(gdx,"ov35_hvarea_primforest",select = list(type="level")),dim=3)
      
      ov35_prod_other <- dimSums(readGDX(gdx,"ov35_prod",select = list(type="level"))[,,"other"],dim=3)
      ov35_hvarea_other <- dimSums(readGDX(gdx,"ov35_hvarea_other",select = list(type="level")),dim=3)
      
      #### Yield calculations
      
      ## Plantations
      yield_forestry <- ov32_prod_forestry/ov32_hvarea_forestry
      if(any(is.na(range(yield_forestry)))){
        yield_forestry[is.na(yield_forestry)] <- 0
      }
      if(any(is.infinite(range(yield_forestry)))){
        div0 <- where(ov32_prod_forestry != 0 & ov32_hvarea_forestry == 0)$true$regions
        yield_forestry[is.infinite(yield_forestry)] <- 0
      }
      
      ## Secondary forest
      yield_secdf <- ov35_prod_secdf/ov35_hvarea_secdf
      if(any(is.na(range(yield_secdf)))){
        yield_secdf[is.na(yield_secdf)] <- 0
      }
      
      if(any(is.infinite(range(yield_secdf)))){
        div0 <- where(ov35_prod_secdf != 0 & ov35_hvarea_secdf == 0)$true$regions
        yield_secdf[is.infinite(yield_secdf)] <- 0
      }
      
      ## Primary forest
      yield_primf <- ov35_prod_primf/ov35_hvarea_primf
      if(any(is.na(range(yield_primf)))){
        yield_primf[is.na(yield_primf)] <- 0
      }
      
      if(any(is.infinite(range(yield_primf)))){
        div0 <- where(ov35_prod_primf != 0 & ov35_hvarea_primf == 0)$true$regions
        yield_primf[is.infinite(yield_primf)] <- 0
      }
      
      ## Other land
      yield_other <- ov35_prod_other/ov35_hvarea_other
      if(any(is.na(range(yield_other)))){
        yield_other[is.na(yield_other)] <- 0
      }
      
      if(any(is.infinite(range(yield_other)))){
        div0 <- where(ov35_prod_other != 0 & ov35_hvarea_other == 0)$true$regions
        yield_other[is.infinite(yield_other)] <- 0
      }
      
      a <- mbind(setNames(yield_forestry,"Forestry"), 
                 setNames(yield_secdf,"Secondary forest"),
                 setNames(yield_primf,"Primary forest"),
                 setNames(yield_other,"Other land"))
    } else if (level == "regglo"){
      #### Production and harvest area calculations
      ov32_prod_forestry <- dimSums(readGDX(gdx,"ov32_prod",select = list(type="level")),dim=3)
      ov32_prod_forestry <- superAggregate(data = ov32_prod_forestry,aggr_type = "sum",level = "regglo")
      ov32_hvarea_forestry <- dimSums(readGDX(gdx,"ov32_hvarea_forestry",select = list(type="level")),dim=3)
      ov32_hvarea_forestry <- superAggregate(data = ov32_hvarea_forestry ,aggr_type = "sum",level = "regglo")
      
      ov35_prod_secdf <- dimSums(readGDX(gdx,"ov35_prod",select = list(type="level"))[,,"secdforest"],dim=3)
      ov35_prod_secdf <- superAggregate(data = ov35_prod_secdf,aggr_type = "sum",level = "regglo")
      ov35_hvarea_secdf <- dimSums(readGDX(gdx,"ov35_hvarea_secdforest",select = list(type="level")),dim=3)
      ov35_hvarea_secdf <- superAggregate(data = ov35_hvarea_secdf ,aggr_type = "sum",level = "regglo")
      
      ov35_prod_primf <- dimSums(readGDX(gdx,"ov35_prod",select = list(type="level"))[,,"primforest"],dim=3)
      ov35_prod_primf <- superAggregate(data = ov35_prod_primf,aggr_type = "sum",level = "regglo")
      ov35_hvarea_primf <- dimSums(readGDX(gdx,"ov35_hvarea_primforest",select = list(type="level")),dim=3)
      ov35_hvarea_primf <- superAggregate(data = ov35_hvarea_primf ,aggr_type = "sum",level = "regglo")
      
      ov35_prod_other <- dimSums(readGDX(gdx,"ov35_prod",select = list(type="level"))[,,"other"],dim=3)
      ov35_prod_other <- superAggregate(data = ov35_prod_other,aggr_type = "sum",level = "regglo")
      ov35_hvarea_other <- dimSums(readGDX(gdx,"ov35_hvarea_other",select = list(type="level")),dim=3)
      ov35_hvarea_other <- superAggregate(data = ov35_hvarea_other ,aggr_type = "sum",level = "regglo")
      
      #### Yield calculations
      
      ## Plantations
      yield_forestry <- ov32_prod_forestry/ov32_hvarea_forestry
      if(any(is.na(range(yield_forestry)))){
        yield_forestry[is.na(yield_forestry)] <- 0
      }
      
      if(any(is.infinite(range(yield_forestry)))){
        div0 <- where(ov32_prod_forestry != 0 & ov32_hvarea_forestry == 0)$true$regions
        yield_forestry[is.infinite(yield_forestry)] <- 0
      }
      
      ## Secondary forest
      yield_secdf <- ov35_prod_secdf/ov35_hvarea_secdf
      if(any(is.na(range(yield_secdf)))){
        yield_secdf[is.na(yield_secdf)] <- 0
      }
      
      if(any(is.infinite(range(yield_secdf)))){
        div0 <- where(ov35_prod_secdf != 0 & ov35_hvarea_secdf == 0)$true$regions
        yield_secdf[is.infinite(yield_secdf)] <- 0
      }
      
      ## Primary forest
      yield_primf <- ov35_prod_primf/ov35_hvarea_primf
      if(any(is.na(range(yield_primf)))){
        yield_primf[is.na(yield_primf)] <- 0
      }
      
      if(any(is.infinite(range(yield_primf)))){
        div0 <- where(ov35_prod_primf != 0 & ov35_hvarea_primf == 0)$true$regions
        yield_primf[is.infinite(yield_primf)] <- 0
      }
      
      ## Other land
      yield_other <- ov35_prod_other/ov35_hvarea_other
      if(any(is.na(range(yield_other)))){
        yield_other[is.na(yield_other)] <- 0
      }
      
      if(any(is.infinite(range(yield_other)))){
        div0 <- where(ov35_prod_other != 0 & ov35_hvarea_other == 0)$true$regions
        yield_other[is.infinite(yield_other)] <- 0
      }
      
      a <- mbind(setNames(yield_forestry,"Forestry"), 
                 setNames(yield_secdf,"Secondary forest"),
                 setNames(yield_primf,"Primary forest"),
                 setNames(yield_other,"Other land"))
    } else {stop("Resolution not recognized. Select cell or regglo as level. NULL returned.")}
  } else if (yield_type== "establishment"){
    if (level == "regglo"){
    #### Production and harvest area calculations
    vm_prod_future_reg_ff <- dimSums(readGDX(gdx,"ov_prod_future_reg_ff",select = list(type="level")),dim=3)
    pcm_production_ratio_future <- readGDX(gdx,"pcm_production_ratio_future")
    future_timber_demand_estb <- vm_prod_future_reg_ff * pcm_production_ratio_future
    
    future_timber_demand_estb <- superAggregate(data = future_timber_demand_estb, aggr_type = "sum",level = "regglo")
    estb_ac0 <- superAggregate(data = dimSums(readGDX(gdx,"ov32_land",select = list(type="level"))[,,"plant"][,,"ac0"],dim=3), aggr_type = "sum", level = "regglo")
    #### Yield calculations
    
    ## Plantations
    yield_estb <- future_timber_demand_estb/estb_ac0
    if(any(is.na(range(yield_estb)))){
     yield_estb[is.na(yield_estb)] <- 0
    }
    
    if(any(is.infinite(range(yield_estb)))){
     div0 <- where(future_timber_demand_estb != 0 & estb_ac0 == 0)$true$regions
     yield_estb[is.infinite(yield_estb)] <- 0
    }
    
    a <- setNames(yield_estb,"Forestry")
    } else if(level == "cell"){
      stop("Establishment yields cannot be calculated on cell resolution due to future demand being on regional level. Use level as regglo.")
    } else {stop("Resolution not recognized. Select regglo as level. NULL returned.")}
    
  } else{stop("Function call parameter not recognized. Select harvest or establishment as type.")}
  
  out(a,file)
}