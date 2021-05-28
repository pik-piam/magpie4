#' @title TimberProductionVolumetric
#' @description reads timber production out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Forest demand for timber production
#' @return Forest demand for timber production
#' @author Abhijeet Mishra
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie setCells
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- TimberProductionVolumetric(gdx)
#'   }

TimberProductionVolumetric <- function(gdx, file=NULL, level="regglo"){
  a <- NULL
  kforestry <- readGDX(gdx,"kforestry")
  
  if (level %in% c("reg","regglo")){
    ov_prod <- readGDX(gdx,"ov_prod_reg",select=list(type="level"))[,,kforestry]
    if(level == "regglo") {
      ov_prod <- mbind(ov_prod,dimSums(ov_prod,dim=1))
    }
    ov_prod[,,"wood"] <- ov_prod[,,"wood"] / 0.6
    ov_prod[,,"woodfuel"] <- ov_prod[,,"woodfuel"] / 0.3
    if("constr_wood" %in% getNames(ov_supply)) ov_supply[,,"constr_wood"] <- ov_supply[,,"constr_wood"] / 0.6
    a <- ov_prod
  } else if (level == "cell"){
    stop("Resolution not recognized. Select regglo as level. NULL returned.")
  }

  out(a,file)
}