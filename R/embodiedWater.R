#' @title embodiedWater
#' @description Consumption-based (embodied) water footprint using the
#'   column-normalised Kastner allocation in \code{\link{embodiedResourceKastner}}.
#'   Non-negative and closes globally to total water use. Crop water is allocated
#'   through primary-equivalent (feed-traced) trade; livestock water through
#'   direct trade.
#'
#' @export
#' @param gdx GDX file
#' @param file optional file name to write the result with \code{write.magpie}
#' @param level regional aggregation level (only "reg" supported)
#' @param type "production", "consumption", "trade", or "all" (default)
#' @param waterType "consumption" (default) or "withdrawal"
#' @param bilateral logical; if TRUE return bilateral (exporter.importer) flows
#' @param secdToFeed logical; if TRUE (default) move the processed-then-fed share
#'   (e.g. soybean -> oilcake -> feed) from the secd pathway to the feed pathway,
#'   so the Livestock pathway captures all crop products that end up as feed. See
#'   \code{\link{embodiedResourceKastner}}.
#' @param reassignLivestock logical; if TRUE (default) move every livestock
#'   product's whole footprint into the feed (Livestock) pathway. See
#'   \code{\link{embodiedResourceKastner}}.
#' @return MAgPIE object (region, year, accounting.pathway.product), or bilateral.
#' @author David M Chen
#' @seealso \code{\link{embodiedResourceKastner}}, \code{\link{embodiedWater}}
#' @importFrom magclass mbind

embodiedWater <- function(gdx, file = NULL, level = "reg", type = "all",
                                 waterType = "consumption", bilateral = FALSE, secdToFeed = TRUE,
                                 reassignLivestock = TRUE) {
  waterUse <- mbind(
    water_usage(gdx, level = level, users = "kcr", sum = FALSE, digits = 10, abstractiontype = waterType),
    water_usage(gdx, level = level, users = "kli", sum = FALSE, digits = 10, abstractiontype = waterType))
  return(embodiedResourceKastner(gdx, resource = waterUse, file = file, level = level,
                          type = type, bilateral = bilateral, secdToFeed = secdToFeed,
                          reassignLivestock = reassignLivestock))
}
