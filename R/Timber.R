#' @title Timber
#' @description reads timber demand out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Forest demandfor timber production
#' @return Forest demandfor timber production
#' @author Abhijeet Mishra
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie setCells
#' @importFrom luscale superAggregate
#' @importFrom madrat toolMappingFile
#' @examples
#' 
#'   \dontrun{
#'     x <- Timber(gdx)
#'   }

Timber <- function(gdx, file=NULL, level="regglo"){
  a <- NULL
  kforestry <- readGDX(gdx,"kforestry")
  if (level %in% c("reg","regglo")){
    f73_volumetric_conversion <- readGDX(gdx,"f73_volumetric_conversion")
    ov_supply <- readGDX(gdx, "ov_supply", select=list(type="level"))[,,kforestry] / f73_volumetric_conversion
    ov_supply <- superAggregate(data = ov_supply,aggr_type = "sum",level = level)
    
    ov_prod <- readGDX(gdx, "ov_prod", select=list(type="level"))[,,kforestry] / f73_volumetric_conversion
    ov_prod <- superAggregate(data = ov_prod,aggr_type = "sum",level = level)
    
    a <- mbind(add_dimension(x = ov_supply,dim = 3.1,nm = "Demand"),add_dimension(x = ov_prod,dim = 3.1,nm = "Production"))
  } else if (level == "cell"){
    stop("Resolution not recognized. Select reg or regglo as level. NULL returned.")
  }
  
  out(a,file)
}