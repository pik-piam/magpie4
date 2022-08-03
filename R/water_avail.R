#' @title water_avail
#' @description reads available water from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx   GDX file
#' @param file  a file name the output should be written to using write.magpie
#' @param level spatial level of aggregation: "cell" (cellular),
#'              "reg" (regional), "glo" (global), "regglo" (regional and global)
#'              or any other aggregation level defined in superAggregate
#' @param sources Vector of water sources that shall be obtained. NULL for all sources
#' @param sum     Sum the contribution of different sources (TRUE) or display them individually (FALSE)
#' @param digits  integer. For rounding of the return values
#' @param dir     directory for files necessary for disaggregation
#' @return A MAgPIE object containing the available water (km^3)
#' @author Markus Bonsch, Felicitas Beier
#' @examples
#' \dontrun{
#' x <- water_avail(gdx)
#' }

water_avail <- function(gdx, file = NULL, level = "reg", dir = ".",
                        sources = NULL, sum = TRUE, digits = 4) {

  x <- readGDX(gdx, "ov43_watavail", "ov_watavail", "ovm_watavail",
               format = "first_found")[, , "level"]

  if (!is.null(sources)) {
    x <- x[, , sources]
  }

  x <- dimSums(x, dim = 3.2)

  if (sum) {
    x <- dimSums(x, dim = 3.1)
  }

  # (dis-)aggregation
  if (level == "grid") {
    stop("disaggregation to grid cell level not yet implemented.
         weight missing.")
  } else {
    x <- gdxAggregate(gdx, x, to = level, absolute = TRUE,
                      weight = NULL, dir = dir)
  }

  # from mio m^3 to km^3
  x <- x / 1000

  return(round(x, digits))
}
