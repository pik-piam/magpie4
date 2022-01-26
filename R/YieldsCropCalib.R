#' @title YieldsCropCalib
#' @description Reads potential yields after calibration
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation
#' @param dir directory for disaggregation to grid level
#' @return A MAgPIE object containing values of potential yields after the calibration routines
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom magpiesets findset
#' @importFrom magclass magpiesort
#' @importFrom luscale speed_aggregate
#' @examples
#' \dontrun{
#' x <- YieldsCropCalib(gdx)
#' }
#'
YieldsCropCalib <- function(gdx, file = NULL, level = "cell", dir = ".") {

  kcr <- findset("kcr")
  t <- readGDX(gdx, "t")

  if (level %in% c("cell", "glo", "reg", "regglo")) {
  out <- readGDX(gdx, "i14_yields_calib")[, t, kcr]

  weight <- out

  # The +0.000001 is added as a small area for crops with zero values in fm_croparea.
  # Otherwise yields for begr and betr are zero.

  area <- readGDX(gdx, "fm_croparea")[, 1995, ] + 0.000001
  weight[, , ] <- area

  if (level != "cell") out <- gdxAggregate(gdx, out, weight = weight, to = level, absolute = FALSE) else out

  } else if (level == "grid") {

    out <- readGDX(gdx, "i14_yields_calib")[, t, kcr]

    out <- gdxAggregate(gdx, out, weight = NULL, to = "grid", absolute = FALSE, dir = dir)
  } else {
    stop("Level not recognized")
  }

  out(out, file)
}
