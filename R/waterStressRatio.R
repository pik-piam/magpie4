#' @title waterStressRatio
#'
#' @description calculates water stress ratio from water availability and water demand in MAgPIE.
#'              Water stress ratio is the ratio of water withdrawals (in the growing period)
#'              to water availability (in the growing period)
#'
#' @param gdx         GDX file
#' @param file        a file name the output should be written to using write.magpie
#' @param level       spatial level of aggregation: "cell" (cellular), "reg" (regional),
#'                    "glo" (global), "regglo" (regional and global) or
#'                    "grid" (grid cell)
#' @param dir         for gridded outputs:
#'                    magpie output directory which contains a mapping file (rds) for disaggregation
#'
#' @return MAgPIE object
#'
#' @export
#'
#' @author Felicitas Beier
#' @examples
#'
#'   \dontrun{
#'     x <- waterStressRatio(gdx)
#'   }
#'

waterStressRatio <- function(gdx, file = NULL, level = "cell", dir = ".") {

  # water use by sector in the growing period (in km3/yr)
  wateruse      <- water_usage(gdx, level = "cell", users = "sectors", digits = 15)
  wwHuman       <- dimSums(wateruse[, , "ecosystem", invert = TRUE], dim = 3)
  # water availability per cluster in the growing period (in km3/yr)
  watAvl        <- water_avail(gdx, level = "cell", sum = TRUE, digits = 15)

  scarcity      <- wwHuman / watAvl

  # (dis)aggregation based on chosen level
  out <- gdxAggregate(gdx = gdx, x = scarcity, dir = dir,
                      weight = "water_avail", sum = TRUE,
                      to = level, absolute = FALSE)

  return(out)
}
