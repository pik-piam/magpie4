#' @title fallow
#' @description calculates fallow land (Mha) from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx   GDX file
#' @param level aggregation level, reg, glo or regglo, cell or grid
#' @param debugMode debug mode TRUE makes some consistency checks
#'                  between estimates for different resolutions
#' @author Benjamin Leon Bodirsky

#' @examples
#' \dontrun{
#' x <- fallow(gdx)
#' }
#'
fallow <- function(gdx, level = "reg", debugMode = FALSE) {

  fallow <- readGDX(gdx, "ov_fallow", react = "silent", select = list(type = "level"))

  if (!is.null(fallow)) {
    fallow <- setNames(fallow, "crop_fallow")
  } else {
    fallow <- setNames(land(gdx, types = "crop", level = "cell"), "crop_fallow") * 0
  }

  if (debugMode) {
    .checkFallowConsistency(gdx, level = "cell",
                            msg = "inconsistency on cluster level. cropland<>croparea+treeCover+fallow")
  }

  out <- gdxAggregate(gdx = gdx, x = fallow, weight = "land",
                      types = "crop", to = level, absolute = TRUE)

  if (debugMode) {
    .checkFallowConsistency(gdx, level = level,
                            msg = "inconsistency on disaggregated level. cropland<>croparea+treeCover+fallow")
  }

  return(out)
}

.checkFallowConsistency <- function(gdx, level, msg) {
  cropland   <- land(gdx, types = "crop", level = level)
  croparea   <- croparea(gdx, product_aggr = TRUE, level = level)
  fallowLand <- fallow(gdx, level = level)
  treeCover  <- croplandTreeCover(gdx, level = level)

  if (sum(abs(cropland - croparea - treeCover - fallowLand)) > 0.1) {
    stop(msg)
  }
}
