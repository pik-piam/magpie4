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


  if (level %in% c("cell", "glo", "reg", "regglo")) {
    kcr <- findset("kcr")
    t <- readGDX(gdx, "t")
    out <- readGDX(gdx, "f14_yields")[, t, kcr]

    weight <- out

    # The +0.000001 is added as a small area for begr and betr, which is zero in fm_croparea.
    # Otherwise yields for begr and betr are zero.

    area <- readGDX(gdx, "fm_croparea")[, 1995, ] + 0.000001
    weight[, , ] <- area

    if (level != "cell") out <- gdxAggregate(gdx, out, weight = weight, to = level, absolute = FALSE) else out

  } else if (level == "grid") {

    kcr <- findset("kcr")
    t <- readGDX(gdx, "t")
    out <- readGDX(gdx, "f14_yields")[, t, kcr]

    out <- gdxAggregate(gdx, out, weight = NULL, to = "grid", absolute = FALSE)
  } else {
    stop("Level not recognized")
  }

  out(out, file)

}
