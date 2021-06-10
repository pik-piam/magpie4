#' @title YieldsCropCalib
#' @description Reads potential yields after calibration
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @return A MAgPIE object containing values of potential yields after the calibration routines
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' x <- YieldsCropCalib(gdx)
#' }
#'
YieldsCropCalib <- function(gdx, file = NULL, level = "cell", dir = ".") {

  kcr <- findset("kcr")
  out <- readGDX(gdx, "i14_yields_calib")[, , kcr]

  if (level != "cell") out <- gdxAggregate(gdx, out, weight = "croparea", to = level, absolute = FALSE, dir = dir)


  out(out, file)
}
