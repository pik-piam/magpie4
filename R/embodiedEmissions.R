#' @title embodiedEmissions
#' @description Consumption-based (embodied) emissions footprint using the
#'   column-normalised Kastner allocation in \code{\link{embodiedResourceKastner}}.
#'   Non-negative and closes globally to total emissions. Pollutants are
#'   aggregated to a single CO2-equivalent value per product (via \code{unit});
#'   crop emissions are allocated through primary-equivalent trade, livestock
#'   emissions through direct trade.
#'
#' @export
#' @param gdx GDX file
#' @param file optional file name to write the result with \code{write.magpie}
#' @param level regional aggregation level (only "reg" supported)
#' @param type "production", "consumption", "trade", or "all" (default)
#' @param unit GWP metric passed to \code{productEmissions} (default "GWP100AR6")
#' @param bilateral logical; if TRUE return bilateral (exporter.importer) flows
#' @param secdToFeed logical; if TRUE (default) move the processed-then-fed share
#'   (e.g. soybean -> oilcake -> feed) from the secd pathway to the feed pathway,
#'   so the Livestock pathway captures all crop products that end up as feed. See
#'   \code{\link{embodiedResourceKastner}}.
#' @param reassignLivestock logical; if TRUE (default) move every livestock
#'   product's whole footprint into the feed (Livestock) pathway. See
#'   \code{\link{embodiedResourceKastner}}.
#' @return MAgPIE object (region, year, accounting.pathway.product) in Mt CO2eq.
#' @author David M Chen
#' @seealso \code{\link{embodiedResourceKastner}}, \code{\link{embodiedEmissions}}
#' @importFrom magclass dimSums

embodiedEmissions <- function(gdx, file = NULL, level = "reg", type = "all",
                                     unit = "GWP100AR6", bilateral = FALSE, secdToFeed = TRUE,
                                     reassignLivestock = TRUE) {
  emis <- productEmissions(gdx, level = "reg", unit = unit, perTonne = FALSE)  # pollutant.product
  emis <- dimSums(emis, dim = 3.1)                                             # -> product (CO2eq)
  return(embodiedResourceKastner(gdx, resource = emis, file = file, level = level,
                          type = type, bilateral = bilateral, secdToFeed = secdToFeed,
                          reassignLivestock = reassignLivestock))
}
