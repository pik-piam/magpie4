#' @title water_AEI
#' @description reads area equipped for irrigation from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level spatial level of aggregation: "cell" (cellular), "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing the area equipped for irrigation (Mha)
#' @author Markus Bonsch
#' @examples
#' 
#'   \dontrun{
#'     x <- water_AEI(gdx)
#'   }
#' 

water_AEI <- function(gdx, file=NULL, level="reg") {
  x <- readGDX(gdx,"ov41_AEI","ov_AEI","ovm_AEI", select = list(type="level"))
  if(is.null(x)) {
    warning("Area equipped for irrigation cannot be calculated as area data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }
  if(level!="cell") x <- superAggregate(x,aggr_type="sum",level=level)
  getNames(x) <- "AEI"
  out(x,file)
}
