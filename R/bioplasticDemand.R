#' @title bioplasticDemand
#' @description returns demand for bioplastic or demand for substrate for bioplastic production
#'
#' @export
#'
#' @param gdx GDX file
#' @param type "bioplastic" for bioplastic demand, "substrate" for biomass demand as substrate for bioplastic production
#' @param detail only relevant for type = "substrate". If TRUE, substrate demand is disaggregated by crop type, if
#' FALSE only the aggregated demand is reported.
#' @param level spatial aggregation to report bioplastic/substrate demand (only "reg" or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' x <- bioplasticDemand(gdx)
#' }

bioplasticDemand <- function(gdx, type = "bioplastic", detail = FALSE, level = "regglo", file = NULL) {

  if (type == "bioplastic") {
    x <- readGDX(gdx, "p62_dem_bioplastic", react = "silent")
  } else if (type == "substrate") {
    x <- readGDX(gdx, "p62_bioplastic_substrate", react = "silent")
  } else {
    stop("Type not supported.")
  }

  if (!(level %in% c("reg", "glo", "regglo") || isCustomAggregation(level))) {
    stop("Spatial aggregation level not supported.")
  }

  if (!is.null(x)) {
    if (isFALSE(detail)) x <- dimSums(x, dim = 3)
    x <- superAggregateX(x, "sum", level = level)
  }

  out(x, file)
}
