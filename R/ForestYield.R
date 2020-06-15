#' @title ForestYield
#' @description reads timber yield out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
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

ForestYield <- function(gdx, file=NULL, level="cell"){
  a <- NULL
  
  if(max(readGDX(gdx,"ov_forestry_reduction")[,,"level"])>0){
    if (level == "cell"){
      #### Production and harvest area calculations
      ov73_prod_forestry <-dimSums(readGDX(gdx,"ov73_prod_forestry",select = list(type="level")),dim=3)
      ov73_hvarea_forestry <- dimSums(readGDX(gdx,"ov73_hvarea_forestry",select = list(type="level")),dim=3)
      
      ov73_prod_natveg_secdf <- dimSums(readGDX(gdx,"ov73_prod_natveg",select = list(type="level"))[,,"secdforest"],dim=3)
      ov_hvarea_secdf <- dimSums(readGDX(gdx,"ov_hvarea_secdforest",select = list(type="level")),dim=3)
      
      ov73_prod_natveg_primf <- dimSums(readGDX(gdx,"ov73_prod_natveg",select = list(type="level"))[,,"primforest"],dim=3)
      ov_hvarea_primf <- dimSums(readGDX(gdx,"ov_hvarea_primforest",select = list(type="level")),dim=3)
      
      ov73_prod_natveg_other <- dimSums(readGDX(gdx,"ov73_prod_natveg",select = list(type="level"))[,,"other"],dim=3)
      ov_hvarea_other <- dimSums(readGDX(gdx,"ov73_hvarea_other",select = list(type="level")),dim=3)
      
      #### Yield calculations
      
      ## Plantations
      yield_forestry <- ov73_prod_forestry/ov73_hvarea_forestry
      if(any(is.na(range(yield_forestry)))){
        yield_forestry[is.na(yield_forestry)] <- 0
      }
      if(any(is.infinite(range(yield_forestry)))){
        div0 <- where(ov73_prod_forestry != 0 & ov73_hvarea_forestry == 0)$true$regions
        yield_forestry[is.infinite(yield_forestry)] <- 0
      }
      
      ## Secondary forest
      yield_secdf <- ov73_prod_natveg_secdf/ov_hvarea_secdf
      if(any(is.na(range(yield_secdf)))){
        yield_secdf[is.na(yield_secdf)] <- 0
      }
      
      if(any(is.infinite(range(yield_secdf)))){
        div0 <- where(ov73_prod_natveg_secdf != 0 & ov_hvarea_secdf == 0)$true$regions
        yield_secdf[is.infinite(yield_secdf)] <- 0
      }
      
      ## Primary forest
      yield_primf <- ov73_prod_natveg_primf/ov_hvarea_primf
      if(any(is.na(range(yield_primf)))){
        yield_primf[is.na(yield_primf)] <- 0
      }
      
      if(any(is.infinite(range(yield_primf)))){
        div0 <- where(ov73_prod_natveg_primf != 0 & ov_hvarea_primf == 0)$true$regions
        yield_primf[is.infinite(yield_primf)] <- 0
      }
      
      ## Other land
      yield_other <- ov73_prod_natveg_other/ov_hvarea_other
      if(any(is.na(range(yield_other)))){
        yield_other[is.na(yield_other)] <- 0
      }
      
      if(any(is.infinite(range(yield_other)))){
        div0 <- where(ov73_prod_natveg_other != 0 & ov_hvarea_other == 0)$true$regions
        yield_other[is.infinite(yield_other)] <- 0
      }
      
      a <- mbind(setNames(yield_forestry,"Forestry"), 
                 setNames(yield_secdf,"Secondary forest"),
                 setNames(yield_primf,"Primary forest"),
                 setNames(yield_other,"Other land"))
    } else if (level == "regglo" | level == "reg"){
      #### Production and harvest area calculations
      ov73_prod_forestry <- dimSums(readGDX(gdx,"ov73_prod_forestry",select = list(type="level")),dim=3)
      ov73_prod_forestry <- superAggregate(data = ov73_prod_forestry,aggr_type = "sum",level = level)
      ov73_hvarea_forestry <- dimSums(readGDX(gdx,"ov73_hvarea_forestry",select = list(type="level")),dim=3)
      ov73_hvarea_forestry <- superAggregate(data = ov73_hvarea_forestry ,aggr_type = "sum",level = level)
      
      ov73_prod_natveg_secdf <- dimSums(readGDX(gdx,"ov73_prod_natveg",select = list(type="level"))[,,"secdforest"],dim=3)
      ov73_prod_natveg_secdf <- superAggregate(data = ov73_prod_natveg_secdf,aggr_type = "sum",level = level)
      ov_hvarea_secdf <- dimSums(readGDX(gdx,"ov_hvarea_secdforest",select = list(type="level")),dim=3)
      ov_hvarea_secdf <- superAggregate(data = ov_hvarea_secdf ,aggr_type = "sum",level = level)
      
      ov73_prod_natveg_primf <- dimSums(readGDX(gdx,"ov73_prod_natveg",select = list(type="level"))[,,"primforest"],dim=3)
      ov73_prod_natveg_primf <- superAggregate(data = ov73_prod_natveg_primf,aggr_type = "sum",level = level)
      ov_hvarea_primf <- dimSums(readGDX(gdx,"ov_hvarea_primforest",select = list(type="level")),dim=3)
      ov_hvarea_primf <- superAggregate(data = ov_hvarea_primf ,aggr_type = "sum",level = level)
      
      ov73_prod_natveg_other <- dimSums(readGDX(gdx,"ov73_prod_natveg",select = list(type="level"))[,,"other"],dim=3)
      ov73_prod_natveg_other <- superAggregate(data = ov73_prod_natveg_other,aggr_type = "sum",level = level)
      ov_hvarea_other <- dimSums(readGDX(gdx,"ov73_hvarea_other",select = list(type="level")),dim=3)
      ov_hvarea_other <- superAggregate(data = ov_hvarea_other ,aggr_type = "sum",level = level)
      
      #### Yield calculations
      
      ## Plantations
      yield_forestry <- ov73_prod_forestry/ov73_hvarea_forestry
      if(any(is.na(range(yield_forestry)))){
        yield_forestry[is.na(yield_forestry)] <- 0
      }
      
      if(any(is.infinite(range(yield_forestry)))){
        div0 <- where(ov73_prod_forestry != 0 & ov73_hvarea_forestry == 0)$true$regions
        yield_forestry[is.infinite(yield_forestry)] <- 0
      }
      
      ## Secondary forest
      yield_secdf <- ov73_prod_natveg_secdf/ov_hvarea_secdf
      if(any(is.na(range(yield_secdf)))){
        yield_secdf[is.na(yield_secdf)] <- 0
      }
      
      if(any(is.infinite(range(yield_secdf)))){
        div0 <- where(ov73_prod_natveg_secdf != 0 & ov_hvarea_secdf == 0)$true$regions
        yield_secdf[is.infinite(yield_secdf)] <- 0
      }
      
      ## Primary forest
      yield_primf <- ov73_prod_natveg_primf/ov_hvarea_primf
      if(any(is.na(range(yield_primf)))){
        yield_primf[is.na(yield_primf)] <- 0
      }
      
      if(any(is.infinite(range(yield_primf)))){
        div0 <- where(ov73_prod_natveg_primf != 0 & ov_hvarea_primf == 0)$true$regions
        yield_primf[is.infinite(yield_primf)] <- 0
      }
      
      ## Other land
      yield_other <- ov73_prod_natveg_other/ov_hvarea_other
      if(any(is.na(range(yield_other)))){
        yield_other[is.na(yield_other)] <- 0
      }
      
      if(any(is.infinite(range(yield_other)))){
        div0 <- where(ov73_prod_natveg_other != 0 & ov_hvarea_other == 0)$true$regions
        yield_other[is.infinite(yield_other)] <- 0
      }
      
      a <- mbind(setNames(yield_forestry,"Forestry"), 
                 setNames(yield_secdf,"Secondary forest"),
                 setNames(yield_primf,"Primary forest"),
                 setNames(yield_other,"Other land"))
    } else {stop("Resolution not recognized. Select cell or reg or regglo as level. NULL returned.")}
    
  } else {cat("Disabeld for magpie run without dynamic forestry. ")}
  
  out(a,file)
}