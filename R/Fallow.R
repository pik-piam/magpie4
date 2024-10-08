#' @title fallow
#' @description calculates fallow land (Mha) from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx   GDX file
#' @param level aggregation level, reg, glo or regglo, cell or grid
#' @param dir   for gridded outputs: magpie output directory
#'              which contains a mapping file (rds)
#' @param debug debug mode TRUE makes some consistency checks
#'              between estimates for different resolutions
#' @author Benjamin Leon Bodirsky

#' @examples
#' \dontrun{
#' x <- fallow(gdx)
#' }
#'
fallow <- function(gdx, level = "reg", dir = ".", debug = FALSE) {

  fallow <- readGDX(gdx, "ov_fallow", react = "silent", select = list(type = "level"))

  if (!is.null(fallow)) {
    fallow <- setNames(fallow, "crop_fallow")
  } else {
    fallow <- land(gdx, level = level, subcategories = "crop", types="crop_fallow", sum = FALSE)
  }

  if (debug) {

    cropland  <- land(gdx, types = "crop", level = "cell")
    croparea <- croparea(gdx, product_aggr = TRUE, level = "cell")
    fallowLand  <- fallow(gdx, level = "cell")
    treeCover <- croplandTreeCover(gdx, level = "cell")

    if (sum(abs(cropland - croparea - treeCover - fallowLand)) > 0.1) {
      stop("inconsistency on cluster level. cropland<>croparea+treeCover+fallow")
    }
  }

  out <- gdxAggregate(gdx = gdx, x = fallow, weight = "croparea",
                      to = level, absolute = TRUE, dir = dir)

  if (debug) {

    cropland  <- land(gdx, types = "crop", level = level)
    croparea <- croparea(gdx, product_aggr = TRUE, level = level)
    fallowLand  <- fallow(gdx, level = level)
    treeCover <- croplandTreeCover(gdx, level = level)

    if (sum(abs(cropland - croparea - treeCover - fallowLand)) > 0.1) {
      stop("inconsistency on disaggregated level. cropland<>croparea+treeCover+fallow")
    }
  }

  return(out)
}
