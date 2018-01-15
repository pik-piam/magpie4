#' @title water_efficiency
#' @description reads Irrigation efficiency from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level spatial level of aggregation: "cell" (cellular), "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing the irrigation efficiency on the requested aggregation level
#' @author Markus Bonsch
#' @examples
#' 
#'   \dontrun{
#'     x <- water_efficiency(gdx)
#'   }
#' 

water_efficiency <- function(gdx, file=NULL, level="reg") {
  x <- readGDX(gdx,"ov42_irrig_eff","ov43_irrig_eff","ov17_irrig_eff", format="first_found")[,,"level"]
  if(is.null(x)) {
    warning("Irrigation efficiency cannot be calculated as data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }
  if(level!="cell") x <- superAggregate(x,aggr_type="mean",level=level)
  names(dimnames(x))[3]<-""
  dimnames(x)[[3]]<-"irrigation efficiency"
  out(x,file)
}
