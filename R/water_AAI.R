#' @title water_AAI
#' @description reads area actually irrigated from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using \code{\link[magclass]{write.magpie}}. See \code{\link[magclass]{write.magpie}} for supported file types
#' @param level spatial level of aggregation: "cell" (cellular), "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing the area actually irrigated (Mha)
#' @author Stephen Wirth, Anne Biewald
#' @examples
#' 
#'   \dontrun{
#'     x <- water_AEI(gdx)
#'   }
#' 

water_AAI <- function(gdx, file=NULL, level="reg") {
 x <-  croparea(gdx, file=file, level="reg", products="kcr", product_aggr=TRUE, water_aggr=F)[,,"irrigated"]
 # x <- readGDX(gdx,"ov41_AEI","ov_AEI","ovm_AEI", select = list(type="level"))
  if(is.null(x)) {
    warning("Area actually irrigated cannot be calculated as area data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }
  if(level!="cell") x <- superAggregate(x,aggr_type="sum",level=level)
  getNames(x) <- "AAI"
  out(x,file)
}