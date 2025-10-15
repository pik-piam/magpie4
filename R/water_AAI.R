#' @title water_AAI
#' @description reads area actually irrigated from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx   GDX file
#' @param file  a file name the output should be written to using \code{\link[magclass]{write.magpie}}.
#'              See \code{\link[magclass]{write.magpie}} for supported file types
#' @param level spatial level of aggregation: "cell" (cellular),
#'              "reg" (regional), "glo" (global), "regglo" (regional and global)
#'              or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing the area actually irrigated (Mha)
#'
#' @importFrom luscale superAggregate
#' @importFrom magclass getNames
#'
#' @author Stephen Wirth, Anne Biewald, Felicitas Beier
#' @examples
#' \dontrun{
#' x <- water_AEI(gdx)
#' }

water_AAI <- function(gdx, file = NULL, level = "reg") {

  x <- croparea(gdx, file = file, level = "cell",
                products = "kcr", product_aggr = TRUE, water_aggr = FALSE)[, , "irrigated"]

  if (is.null(x)) {
    warning("Area actually irrigated cannot be calculated as
            area data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }

  # (Dis-)Aggregate
  x <- gdxAggregate(gdx, x, to = level, absolute = TRUE,
                      weight = "land", types = "crop")

  getNames(x) <- "AAI"

  out(x, file)
}
