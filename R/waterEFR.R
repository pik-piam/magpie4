#' @title waterEFR
#' @description reads environmental flow requirements (as they enter MAgPIE)
#'              from a MAgPIE gdx file
#'
#' @param gdx    GDX file
#' @param file   a file name the output should be written to using write.magpie
#' @param level  spatial level of aggregation: "cell" (cellular), "reg" (regional),
#'               "glo" (global), "regglo" (regional and global)
#' @param digits integer. For rounding of the return values
#'
#' @return A MAgPIE object containing environmental flow requirements (km^3)
#'
#' @author Felicitas Beier
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- waterEFR(gdx)
#' }
#'
waterEFR <- function(gdx, file = NULL, level = "cell", digits = 4) {

  years <- readGDX(gdx, "t", types = "sets")

  # environmental flow requirements (mio. m^3)
  efr   <- readGDX(gdx, "f42_env_flows")[, years, ]

  ## Disaggregation to 0.5 degree grid cells not available yet      ### @BENNI?
  ## (requires 0.5 degree water availability & also questionable
  ## if it makes sense given that we could then just use the gridded input file)
  # (dis)aggregate using available water as weight
  # avlWater <- read.magpie("lpj_watavail_total_0.5.mz")
  # x <- gdxAggregate(gdx = gdx, x = efr,
  #                  weight = avlWater,
  #                  to = level, absolute = TRUE)

  # Aggregation
  if (level != "cell") {
    efr <- superAggregate(efr, aggr_type = "sum", level = level)
  }

  # from mio m^3 to km^3
  efr <- efr / 1000

  return(round(efr, digits))
}
