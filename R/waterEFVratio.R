#' @title waterEFVratio
#'
#' @description calculates ratio of environmental flow violation volume (EFV) to
#'              water environmental flow requirements (EFR) in MAgPIE.
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
#'     x <- waterEFVratio(gdx)
#'   }
#'

waterEFVratio <- function(gdx, file = NULL, level = "cell", dir = ".") {

  # environmental flow violation volume
  efv <- waterEFViolation(gdx, level = "cell", digits = 15, dir = dir)

  # environmental flow requirements
  efr <- waterEFR(gdx, level = "cell", digits = 15)

  ratio <- efv / efr

  # (dis)aggregation based on chosen level
  out <- gdxAggregate(gdx = gdx, x = ratio, dir = dir,
                      weight = "water_avail", sum = TRUE,
                      to = level, absolute = FALSE)

  return(out)
}
