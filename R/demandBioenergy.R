#' @title demandBioenergy
#' @description reads bioenergy demand from a MAgPIE gdx file
#'  
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param sum 1st and 2nd generation bioenergy demand (FALSE) or total bioenergy demand (TRUE)
#' @param round NULL or number of digits
#' @return A MAgPIE object containing bioenergy demand in EJ/yr
#' @author Jan Philipp Dietrich, Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- demandBioenergy(gdx)
#'   }
#' @importFrom magclass setNames dimSums
#' @export

demandBioenergy <- function(gdx, file=NULL, level="reg", sum=FALSE, round=NULL) {
  biodem <- collapseNames(demand(gdx,level=level,attributes = "ge")[,,"bioenergy"])/1000
  crop2nd <- c("begr","betr")
  crop1st <- setdiff(getNames(biodem),crop2nd)
  b2nd <- setNames(dimSums(biodem[,,crop2nd],dim=3),"2nd generation")
  b1st <- setNames(dimSums(biodem[,,crop1st],dim=3),"1st generation")
  biodem <- mbind(b1st,b2nd)
  if (sum) biodem <- dimSums(biodem,dim=3)
  if (!is.null(round)) biodem <- round(biodem,round)
  out(biodem,file)
} 