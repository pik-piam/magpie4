#' @title YieldsCropCalib
#' @description Reads potential yields after calibration
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation
#' @param tau if TRUE add effect of TAU to the yield
#' @return A MAgPIE object containing values of potential yields after the calibration routines
#' @author Edna Molina Bacca
#' @importFrom magpiesets findset
#' @importFrom magclass magpiesort
#' @examples
#' \dontrun{
#' x <- YieldsCropCalib(gdx)
#' }
#'
YieldsCropCalib <- function(gdx, file = NULL, level = "cell", tau = FALSE) {

  kcr <- readGDX(gdx, "kcr")
  t   <- readGDX(gdx, "t")
  out <- readGDX(gdx, "i14_yields_calib")[, t, kcr]

  if(tau) {
    gsadaptRatio <- suppressWarnings(readGDX(gdx, "p14_yields_gsadapt_ratio_cummulative"))
    if(is.null(gsadaptRatio)) gsadaptRatio <- 1
    tau1995 <- readGDX(gdx, "fm_tau1995")
    vmTau   <- readGDX(gdx, "ov_tau", select = list(type="level"))[, , "crop"]
    out     <- collapseDim(out / gsadaptRatio * vmTau / tau1995)
  }

  if (level %in% c("cell", "glo", "reg", "regglo")) {

    # The +0.000001 is added as a small area for crops with zero values in fm_croparea.
    # Otherwise yields for begr and betr are zero.
    weight <- out
    area <- readGDX(gdx, "fm_croparea")[, 1995, ] + 0.000001
    weight[, , ] <- area

    if (level != "cell") out <- gdxAggregate(gdx, out, weight = weight, to = level, absolute = FALSE) else out

  } else if (level == "grid") {

    out <- gdxAggregate(gdx, out, weight = NULL, to = "grid", absolute = FALSE)

  } else {
    stop("Level not recognized")
  }

  out(out, file)
}
