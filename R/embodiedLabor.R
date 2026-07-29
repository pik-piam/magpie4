#' @title embodiedLabor
#' @description Consumption-based (embodied) agricultural-labour footprint using
#'   the column-normalised Kastner allocation in
#'   \code{\link{embodiedResourceKastner}}. Non-negative and closes globally to
#'   total agricultural employment. Crop/pasture labour is allocated through
#'   primary-equivalent trade, livestock labour through direct trade.
#'
#' @export
#' @param gdx GDX file
#' @param file optional file name to write the result with \code{write.magpie}
#' @param level regional aggregation level (only "reg" supported)
#' @param type "production", "consumption", "trade", or "all" (default)
#' @param bilateral logical; if TRUE return bilateral (exporter.importer) flows
#' @param secdToFeed logical; if TRUE (default) move the processed-then-fed share
#'   (e.g. soybean -> oilcake -> feed) from the secd pathway to the feed pathway,
#'   so the Livestock pathway captures all crop products that end up as feed. See
#'   \code{\link{embodiedResourceKastner}}.
#' @param reassignLivestock logical; if TRUE (default) move every livestock
#'   product's whole footprint into the feed (Livestock) pathway. See
#'   \code{\link{embodiedResourceKastner}}.
#' @return MAgPIE object (region, year, accounting.pathway.product) in million people.
#' @author David M Chen
#' @seealso \code{\link{embodiedResourceKastner}}, \code{\link{embodiedLabor}}
#' @importFrom magclass mbind setNames
#' @importFrom gdx2 readGDX

embodiedLabor <- function(gdx, file = NULL, level = "reg", type = "all",
                                 bilateral = FALSE, secdToFeed = TRUE,
                                 reassignLivestock = TRUE) {
  # Employment by product (kcr + kli), plus a pasture share split from labour costs
  employment <- agEmployment(gdx, type = "absolute", detail = "byProduct", level = "reg")
  totalEmpl  <- readGDX(gdx, "ov36_employment", select = list(type = "level"), react = "silent")

  lcPast <- factorCosts(gdx, products = "pasture", level = "reg")[, , "labor_costs", drop = TRUE]
  lcCrop <- factorCosts(gdx, products = "kcr",     level = "reg")[, , "labor_costs", drop = TRUE]
  lcLi   <- factorCosts(gdx, products = "kli",     level = "reg")[, , "labor_costs", drop = TRUE]
  pastShare <- lcPast / (lcPast + lcCrop + lcLi)
  pastShare[!is.finite(pastShare)] <- 0
  emplPast <- setNames(totalEmpl * pastShare, "pasture")
  employment <- mbind(employment, emplPast)

  return(embodiedResourceKastner(gdx, resource = employment, file = file, level = level,
                          type = type, bilateral = bilateral, secdToFeed = secdToFeed,
                          reassignLivestock = reassignLivestock))
}
