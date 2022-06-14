#' @title fallow
#' @description calculates fallow land (Mha) from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level, reg, glo or regglo, cell or grid
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param debug debug mode TRUE makes some consistency checks between estimates for different resolutions.
#' @author Benjamin Leon Bodirsky

#' @examples
#' \dontrun{
#' x <- fallow(gdx)
#' }
#'
fallow <- function(gdx, level = "reg", dir = ".", debug = FALSE) {

  fallow=readGDX(gdx,"ov_fallow",react="silent")
  if(!is.null(fallow)){
    fallow=setNames(fallow[,,"level"],"fallow")
  } else {
    fallow=setNames(land(gdx,types="crop",level="cell"),"fallow")*0
  }

  if(debug) {
    cropland=land(gdx,types="crop",level="cell")
    cropareaX=croparea(gdx,product_aggr = TRUE,level="cell")
    if(sum(abs(cropland-croparea-fallow))>0.1) {stop("inconsistency on cluster level. cropland<>croparea+fallow")}
  }

  out=gdxAggregate(gdx = gdx,x = fallow,weight = 'croparea',to = level,absolute = TRUE,dir = dir)

  if(debug) {
    cropland=land(gdx,types="crop",level=level)
    cropareaX=croparea(gdx,product_aggr = TRUE,level=level)
    if(sum(abs(cropland-croparea-fallow))>0.1) {stop("inconsistency on disaggregated level. cropland<>croparea+fallow")}
  }
  return(out)
}
