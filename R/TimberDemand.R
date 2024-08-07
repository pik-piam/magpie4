#' @title TimberDemand
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
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie setCells
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- TimberDemand(gdx)
#'   }

TimberDemand <- function(gdx, file=NULL, level="regglo"){
  a <- NULL

  if (level == "reg"){
    ov_supply <- collapseNames(readGDX(gdx,"ov_supply")[,,readGDX(gdx,"kforestry")][,,"level"])
    getNames(ov_supply) <- c("Industrial roundwood","Wood fuel")
    ov_supply <- mbind(ov_supply, setCells(dimSums(ov_supply, dim=1),"GLO"))
  } else if (level == "regglo"){
    ov_supply <- collapseNames(readGDX(gdx,"ov_supply")[,,readGDX(gdx,"kforestry")][,,"level"])
    getNames(ov_supply) <- c("Industrial roundwood","Wood fuel")
    ov_supply <- mbind(ov_supply, setCells(dimSums(ov_supply, dim=1),"GLO"))
 } else if (level == "cell"){
    stop("Resolution not recognized. Select regglo as level. NULL returned.")
   }

  a <- ov_supply

  out(a,file)
}
