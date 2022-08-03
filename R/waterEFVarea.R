#' @title waterEFVarea
#' @description calculates area that falls into cluster experiencing
#'              environmental flow violations
#'              from MAgPIE outputs
#'
#' @param gdx    GDX file
#' @param file   a file name the output should be written to using write.magpie
#' @param level  spatial level of aggregation: "cell" (cellular), "reg" (regional),
#'               "glo" (global), "regglo" (regional and global), or
#'               "grid" (for disaggregated output using cropland as weight)
#' @param digits integer. For rounding of the return values
#' @param dir    directory for weight for disaggregation
#'
#' @return A MAgPIE object containing the area under environmental flow violations (Mha)
#'
#' @author Felicitas Beier
#'
#' @importFrom magclass dimSums
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- waterEFVarea(gdx)
#' }

waterEFVarea <- function(gdx, file = NULL, level = "reg", digits = 4, dir = ".") {

  # environmental flow violations
  violations <- waterEFViolation(gdx, level = "cell", digits = 4)
  violations[violations > 0] <- 1

  # irrigated area
  irrigArea  <- water_AAI(gdx, level = "cell")

  x <- irrigArea * violations

  # (dis)aggregate
  out <- gdxAggregate(gdx = gdx, x = x,
                      weight = "water_AAI", dir = dir,
                      to = level, absolute = TRUE)


  return(round(out, digits))
}
