#' @title YieldsCropRaw
#' @description Reads potential yields after calibration
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation
#' @return A MAgPIE object containing values of potential yields after the calibration routines
#' @author Edna Molina Bacca
#' @importFrom magclass magpiesort
#'
#' @examples
#' \dontrun{
#' x <- YieldsCropRaw(gdx)
#' }
#'
YieldsCropRaw <- function(gdx, file = NULL, level = "cell") {

  kcr <- readGDX(gdx, "kcr")
  t   <- readGDX(gdx, "t")

  gsAdaptSwitch <- suppressWarnings(readGDX(gdx, "s14_use_gsadapt"))
  if (is.null(gsAdaptSwitch) || (gsAdaptSwitch == 1)) {
    out <- readGDX(gdx, "f14_yields")[, t, kcr]
  } else if (gsAdaptSwitch == 0) {
    out <- readGDX(gdx, "f14_yields_constgsadapt")[, t, kcr]
  } else {
    stop("Wrong setting for 's14_use_gsadapt'")
  }

  if (level %in% c("cell", "glo", "reg", "regglo")) {
    # The +0.000001 is added as a small area for begr and betr, which is zero in fm_croparea.
    # Otherwise yields for begr and betr are zero.
    weight <- out
    area <- readGDX(gdx, "fm_croparea")[, 1995, ] + 0.000001
    weight[, , ] <- area
    if (level != "cell") {
      out <- gdxAggregate(gdx, out, weight = weight, to = level, absolute = FALSE)
    }
  } else if (level == "grid") {
    out <- gdxAggregate(gdx, out, weight = NULL, to = "grid", absolute = FALSE)
  } else {
    stop("Level not recognized")
  }

  out(out, file)

}
