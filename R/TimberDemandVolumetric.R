#' @title TimberDemandVolumetric
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
#'     x <- TimberDemandVolumetric(gdx)
#'   }

TimberDemandVolumetric <- function(gdx, file=NULL, level="regglo"){
  a <- NULL
  kforestry <- readGDX(gdx,"kforestry")
  if (level %in% c("reg","regglo")){
    ov_supply <- readGDX(gdx, "ov_supply", select=list(type="level"))[,,kforestry]
    ov_supply[,,"wood"] <- ov_supply[,,"wood"] / 0.6
    ov_supply[,,"woodfuel"] <- ov_supply[,,"woodfuel"] / 0.3
    ov_supply <- superAggregate(data = ov_supply,aggr_type = "sum",level = level)
    a <- ov_supply
  } else if (level == "cell"){
    stop("Resolution not recognized. Select regglo as level. NULL returned.")
  }
  
  out(a,file)
}