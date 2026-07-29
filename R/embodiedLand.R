#' @title embodiedLand
#' @description Consumption-based (embodied) land footprint using the
#'   column-normalised Kastner allocation in \code{\link{embodiedResourceKastner}}.
#'   Unlike \code{\link{embodiedLand}} (which uses production + net-trade and can
#'   produce local negatives), this distributes each region's actual cropland and
#'   pasture to consumers, so the consumption footprint is non-negative and
#'   closes globally to total agricultural land.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file optional file name to write the result with \code{write.magpie}
#' @param level regional aggregation level (only "reg" supported)
#' @param type "production", "consumption", "trade", or "all" (default)
#' @param landType "all" (crop + pasture), "crop", or "past"
#' @param bilateral logical; if TRUE return bilateral (exporter.importer) flows
#' @param secdToFeed logical; if TRUE (default) move the processed-then-fed share
#'   (e.g. soybean -> oilcake -> feed) from the secd pathway to the feed pathway,
#'   so the Livestock pathway captures all crop products that end up as feed. See
#'   \code{\link{embodiedResourceKastner}}.
#' @param reassignLivestock logical; if TRUE (default) move every livestock
#'   product's whole footprint into the feed (Livestock) pathway. See
#'   \code{\link{embodiedResourceKastner}}. A no-op for land (no kli land).
#'
#' @return MAgPIE object in Mha. When bilateral=FALSE: (region, year,
#'   accounting.pathway.product). When bilateral=TRUE: (exporter.importer, year,
#'   pathway.product).
#' @author David M Chen
#' @seealso \code{\link{embodiedResourceKastner}}, \code{\link{embodiedLand}}
#' @importFrom magclass getItems mbind

embodiedLand <- function(gdx, file = NULL, level = "reg", type = "all",
                                landType = "all", bilateral = FALSE, secdToFeed = TRUE,
                                reassignLivestock = TRUE) {

  # Resource total per product: cropland (by crop) + pasture
  cropLand <- croparea(gdx, level = level, products = "kcr", product_aggr = FALSE, water_aggr = TRUE)
  pastLand <- land(gdx, level = level, types = "past", subcategories = FALSE)
  getItems(pastLand, dim = 3) <- "pasture"     # match production / trade naming (kve set)
  resource <- mbind(cropLand, pastLand)

  if (landType == "crop") {
    resource <- resource[, , "pasture", invert = TRUE]
  } else if (landType == "past") {
    resource <- resource[, , "pasture"]
  } else if (landType != "all") {
    stop("Invalid landType. Choose 'all', 'crop', or 'past'.")
  }

  out <- embodiedResourceKastner(gdx, resource = resource, file = file, level = level,
                                 type = type, bilateral = bilateral, secdToFeed = secdToFeed,
                                 reassignLivestock = reassignLivestock)
  return(out)
}
