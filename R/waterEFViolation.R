#' @title waterEFViolation
#' @description calculates environmental flow violation volume
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
#' @return A MAgPIE object containing the volume of environmental flow violations (km^3)
#'
#' @author Felicitas Beier
#'
#' @importFrom magclass dimSums
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- waterEFViolation(gdx)
#' }

waterEFViolation <- function(gdx, file = NULL, level = "reg", digits = 4, dir = ".") {

  # human water withdrawals in the growing period (in km^3/yr)
  wwHuman <- dimSums(water_usage(gdx, level = "cell", digits = 15,
                                 seasonality = "grper",
                                 users = "sectors", sum = FALSE)[, , "ecosystem", invert = TRUE],
                     dim = 3)

  # water availability in the growing period in MAgPIE (in km^3/yr)
  watAvl  <- water_avail(gdx, level = "cell", sum = TRUE, digits = 15)

  # environmental flow requirements (in km3/yr)
  efr     <- waterEFR(gdx, level = "cell", digits = 15)

  # Calculate water available for use without violating EFR
  violations <- wwHuman + efr - watAvl
  violations[violations < 0] <- 0

  # (dis)aggregate
  out <- gdxAggregate(gdx = gdx, x = violations,
                      weight = "water_AAI", dir = dir,
                      to = level, absolute = TRUE)

  return(round(out, digits))
}
