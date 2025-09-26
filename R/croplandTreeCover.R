#' @title croplandTreeCover
#' @description calculates tree cover on cropland (Mha) from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx   GDX file
#' @param level aggregation level, reg, glo or regglo, cell or grid
#' @param sum_ac  sum over age classes TRUE / FALSE
#' @param debug debug mode TRUE makes some consistency checks
#'              between estimates for different resolutions
#' @author Florian Humpenoeder

#' @examples
#' \dontrun{
#' x <- fallow(gdx)
#' }
#'
croplandTreeCover <- function(gdx, level = "reg", sum_ac = TRUE, debug = FALSE) {

  croplandTreeCover <- readGDX(gdx, "ov29_treecover", react = "silent", select = list(type = "level"))

  if (!is.null(croplandTreeCover)) {
    if (debug) {

      cropland  <- land(gdx, types = "crop", level = "cell")
      croparea <- croparea(gdx, product_aggr = TRUE, level = "cell")
      fallowLand  <- fallow(gdx, level = "cell")

      if (sum(abs(cropland - croparea - dimSums(croplandTreeCover, dim = "ac") - fallowLand)) > 0.1) {
        stop("inconsistency on cluster level. cropland<>croparea+croplandTreeCover+fallow")
      }
    }
    if (sum_ac) {
      croplandTreeCover <- dimSums(croplandTreeCover, dim = "ac")
      croplandTreeCover <- setNames(croplandTreeCover, "crop_treecover")
    }
  } else {
    croplandTreeCover <- land(gdx, level = level, subcategories = "crop", types="crop_treecover", sum = FALSE)
  }

  out <- gdxAggregate(gdx = gdx, x = croplandTreeCover, weight = "land", subcategories = "crop", types="crop_treecover",
                      to = level, absolute = TRUE)

  return(out)
}
