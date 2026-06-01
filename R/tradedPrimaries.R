#' @title tradedPrimaries
#' @description Converts trade flows into primary product equivalents by tracing
#'              secondary and livestock products back to their primary inputs.
#'              Supports both net trade flows and bilateral trade matrices.
#'              Trade flows are decomposed into three pathways:
#'              (1) direct primary trade, (2) primaries embodied in secondary products,
#'              and (3) primaries needed as feed for livestock.
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param bilateral Logical or NULL. If NULL (default), auto-detects: uses bilateral
#'                  trade matrix (ov21_trade) if present, otherwise falls back to net trade.
#'                  If TRUE, requires bilateral data and warns if not found before falling back.
#'                  If FALSE, always uses net trade flows.
#' @param convFactor Character. When using bilateral trade, determines whether processing
#'                     shares and feed baskets are taken from "exporter" or "importer" region.
#'                     Default is "exporter" (footprint allocation to producing region).
#' @param kastner Logical. If TRUE and bilateral=TRUE, applies Kastner et al. 2011 adjustment
#'                to bilateral trade matrix. Default is TRUE.
#' @param level Regional aggregation level ("reg", "glo", "regglo", or custom). Default is "reg".
#' @param disaggLivestock Logical. If TRUE, the feed pathway retains the livestock product
#'   dimension so that feed crop demands can be traced back to specific animal products.
#'   Output dim 3 will be \code{kli_product.kve_product} for the feed pathway instead of
#'   the aggregated \code{feed.kve_product}. The \code{prim} and \code{secd} pathways are
#'   unchanged (\code{pathway.kve_product}). Default is FALSE.
#'
#' @details When convFactor="exporter", the exporting region's processing pathways and 
#'          feed baskets are used, which is appropriate for production-based footprint accounting.
#'          When convFactor="importer", the importing region's factors are used, which 
#'          maintains backward compatibility with the original tradedPrimaries function.
#'          
#'          For bilateral trade, the output includes full origin-destination detail.
#'          For net trade, the output is aggregated by region.
#'          
#'          When \code{disaggLivestock=TRUE}, callers can assign embodied resources either
#'          to the feed crop (collapse dim 3.1, i.e. the livestock dimension) or to the animal
#'          product (collapse dim 3.2, i.e. the crop dimension).
#'
#' @return MAgPIE object with primary product trade equivalents in dry matter (tDM).
#'         For bilateral trade: dimensions are (exporter.importer, year, pathway.product)
#'         For net trade: dimensions are (region, year, pathway.product)
#'         When disaggLivestock=TRUE: feed items have dimensions kli_product.kve_product
#'         instead of feed.kve_product; prim/secd items are unchanged.
#' @author David M Chen, Kristine Karstens
#' 
#' @importFrom madrat toolConditionalReplace
#' @importFrom gdx2 readGDX
#' @importFrom magclass getItems dimSums collapseNames mbind setNames add_columns new.magpie getYears getRegions getNames add_dimension
#' @examples
#' \dontrun{
#'   # Bilateral trade with exporter's factors (production-based accounting)
#'   x <- tradedPrimaries(gdx, bilateral = TRUE, convFactor = "exporter")
#'   
#'   # Bilateral trade with importer's factors (consumption-based accounting)
#'   x <- tradedPrimaries(gdx, bilateral = TRUE, convFactor = "importer")
#'   
#'   # Net trade (backward compatible with tradedPrimaries)
#'   x <- tradedPrimaries(gdx, bilateral = FALSE)
#' }
#'
#' @export

tradedPrimaries <- function(gdx, 
                            file = NULL, 
                            bilateral = NULL,
                            convFactor = "exporter",
                            kastner = TRUE,
                            level = "reg",
                            disaggLivestock = FALSE) {
  
  # Validate inputs
  if (!convFactor %in% c("exporter", "importer")) {
    stop("convFactor must be either 'exporter' or 'importer'")
  }
  
  # ===========================================================================
  # NESTED HELPERS
  # ===========================================================================
  
  # Apply a regional factor to trade flows.
  # maskDim encodes which spatial dimension to mask for bilateral trade so that
  # the factor is taken from the correct region (exporter or importer).
  # maskDim = NULL means net trade: direct element-wise multiplication.
  .applyRegionalFactor <- function(tradeData, regionalFactor, maskDim = NULL) {
    if (!is.null(maskDim)) {
      getItems(tradeData, dim = maskDim) <- paste0(getItems(tradeData, dim = maskDim), "_tmp")
      result <- tradeData * regionalFactor
      getItems(result, dim = maskDim) <- sub("_tmp$", "", getItems(result, dim = maskDim))
    } else {
      result <- tradeData * regionalFactor
    }
    return(result)
  }
  
  # Iterative livestock-as-feed convergence.
  # Solves: TotalLiDemand = liTrade + liInFeed(TotalLiDemand)
  # Equivalent to geometric series: Total = L + AL + A^2*L + A^3*L + ...
  # maskDim: spatial dimension to mask for bilateral; NULL for net trade.
  .iterativeLiDemand <- function(liTrade, feedBaskets, kli, maskDim = NULL) {
    totalLiDemand <- liTrade
    for (iter in 1:10) {
      allFeedDemands <- .applyRegionalFactor(totalLiDemand, feedBaskets, maskDim)
      kliInFeed      <- intersect(getItems(allFeedDemands, dim = 3.2), kli)
      if (length(kliInFeed) == 0) break
      liInFeed       <- dimSums(allFeedDemands[, , kliInFeed], dim = 3.1)
      liInFeed       <- setNames(liInFeed, paste0("kli_", getNames(liInFeed)))
      if (max(abs(liInFeed), na.rm = TRUE) < 1e-6) break
      totalLiDemand <- totalLiDemand + liInFeed
    }
    return(totalLiDemand)
  }
  
  if (!is.null(bilateral) && !is.logical(bilateral)) {
    stop("bilateral must be TRUE, FALSE, or NULL (auto-detect)")
  }

  # ===========================================================================
  # GET TRADE FLOWS
  # ===========================================================================

  if (!isFALSE(bilateral)) {
    # bilateral=NULL (auto-detect) or bilateral=TRUE (require): try bilateral first
    tradeFlows <- tryCatch({
      tmp <- readGDXBilateral(gdx = gdx, "ov21_trade")[, , "level"]
      collapseNames(tmp)
    }, error = function(e) NULL)

    if (is.null(tradeFlows)) {
      if (isTRUE(bilateral)) {
        warning("tradedPrimaries: bilateral=TRUE but ov21_trade not found - falling back to net trade")
      } else {
        message("tradedPrimaries: bilateral trade data not found, using net trade")
      }
      bilateral  <- FALSE
      tradeFlows <- trade(gdx, type = "net-exports")
    } else {
      bilateral <- TRUE
    }
  } else {
    tradeFlows <- trade(gdx, type = "net-exports")
  }
  
  # ===========================================================================
  # READ COMMON DATA
  # ===========================================================================
  
  kcr  <- readGDX(gdx, "kcr")   # Primary crops
  ksd  <- readGDX(gdx, "ksd")   # Secondary products
  kli  <- readGDX(gdx, "kli")   # Livestock products
  kap  <- readGDX(gdx, "kap")   # Animal products
  kres <- readGDX(gdx, "kres")  # Residues
  kve  <- readGDX(gdx, "kve")   # Land-use activities (primary products for output)
  
  simYears <- getYears(tradeFlows)
  
  # Get primary per secondary conversion factors
  primPerSecd <- primaryPerSecondary(gdx, level = level, allocation = "value")
  primPerSecd <- primPerSecd[, simYears, ]
  
  # Get feed baskets (tDM feed per tDM livestock product)
  feedBaskets <- readGDX(gdx, "im_feed_baskets")
  getNames(feedBaskets, dim = "kap") <- paste0("kli_", getNames(feedBaskets, dim = "kap"))
  feedBaskets <- feedBaskets[, simYears, paste0("kli_", kli)]
  
  if (bilateral) {
    message("Using bilateral trade matrix with ", convFactor, "'s processing/feed factors.")
  } else {
    message("NOTE: Using net trade flows. Processing shares and livestock feed baskets of ",
            "importing regions are used to calculate primary product requirements.")
  }
  
  # maskDim encodes convFactor for bilateral factor application; NULL for net trade
  maskDim <- if (bilateral) (if (convFactor == "exporter") 1.2 else 1.1) else NULL
  
  # ===========================================================================
  # OUTPUT CONTAINER: zero-filled; each pathway writes directly into it
  # ===========================================================================

  regionPairs      <- getItems(tradeFlows, dim = 1)
  primSecdDimNames <- as.vector(outer(c("prim", "secd"), kve, paste, sep = "."))
  feedDimNames     <- as.vector(outer(paste0("kli_", kli), kve, paste, sep = "."))
  primaryDemands   <- new.magpie(regionPairs, simYears,
                                  names = c(primSecdDimNames, feedDimNames), fill = 0)

  # ===========================================================================
  # PRIMARY PATHWAY: Direct trade of primary products
  # ===========================================================================

  kcrInOutput <- intersect(intersect(getItems(tradeFlows, dim = 3), kcr), kve)
  if (length(kcrInOutput) > 0)
    primaryDemands[, , "prim"][, , kcrInOutput] <- tradeFlows[, , kcrInOutput]

  # ===========================================================================
  # SECONDARY PATHWAY: Primaries embodied in secondary products
  # ===========================================================================

  tradedKsdProducts <- intersect(getItems(tradeFlows, dim = 3), ksd)
  if (length(tradedKsdProducts) > 0) {
    secdTrade <- tradeFlows[, , tradedKsdProducts]
    primaryDemandsSecd <- .applyRegionalFactor(secdTrade, primPerSecd[, , tradedKsdProducts], maskDim)
    primaryDemandsSecd <- dimSums(primaryDemandsSecd, dim = 3.1)
    kcrFromSecd <- intersect(getItems(primaryDemandsSecd, dim = 3), kve)
    if (length(kcrFromSecd) > 0)
      primaryDemands[, , "secd"][, , kcrFromSecd] <- primaryDemandsSecd[, , kcrFromSecd]
  }

  # ===========================================================================
  # FEED PATHWAY: Primaries needed as livestock feed
  # Always computed with kli dimension retained; collapsed at end if !disaggLivestock.
  # ===========================================================================

  tradedKliProducts <- intersect(getItems(tradeFlows, dim = 3), kli)
  if (length(tradedKliProducts) > 0) {
    liTrade <- tradeFlows[, , tradedKliProducts]
    getNames(liTrade) <- paste0("kli_", getNames(liTrade))

    totalLiDemand <- .iterativeLiDemand(liTrade, feedBaskets, kli, maskDim)
    feedDemands   <- .applyRegionalFactor(totalLiDemand, feedBaskets, maskDim)

    # Direct primary crop feeds (dim 3 = kli_X.kve)
    kveInFeed <- intersect(getItems(feedDemands, dim = 3.2), kve)
    feedItems <- intersect(getItems(feedDemands[, , kveInFeed], dim = 3), feedDimNames)
    if (length(feedItems) > 0)
      primaryDemands[, , feedItems] <- feedDemands[, , feedItems]

    # Secondary feeds converted to primary equivalents (dim 3 = kli_X.kve after collapse)
    ksdInFeed <- intersect(getItems(feedDemands, dim = 3.2), ksd)
    if (length(ksdInFeed) > 0) {
      feedDemandsSecd <- .applyRegionalFactor(feedDemands[, , ksdInFeed],
                                               primPerSecd[, , ksdInFeed], maskDim)
      feedDemandsSecd <- dimSums(feedDemandsSecd, dim = 3.2)
      feedSecdItems   <- intersect(getItems(feedDemandsSecd, dim = 3), feedDimNames)
      if (length(feedSecdItems) > 0)
        primaryDemands[, , feedSecdItems] <- primaryDemands[, , feedSecdItems] + feedDemandsSecd[, , feedSecdItems]
    }
    feedDemandsKres <- feedDemands[, , kres]  # residues ignored for now
  }
  
  # ===========================================================================
  # KASTNER ADJUSTMENT (bilateral only)
  # ===========================================================================
  
  if (kastner && bilateral) {
    # Sum across pathways for Kastner adjustment, then redistribute
    totalByProduct <- dimSums(primaryDemands, dim = 3.1)
    totalKastner <- tradeKastner(gdx = gdx,
                                 trade = totalByProduct, level = level,
                                 products = getItems(totalByProduct, dim = 3),
                                 attributes = "dm")
    # Calculate pathway shares and redistribute Kastner-adjusted totals
    pathwayShares <- primaryDemands / totalByProduct
    pathwayShares[is.na(pathwayShares)] <- 0
    pathwayShares[is.infinite(pathwayShares)] <- 0
    primaryDemands <- pathwayShares * totalKastner
  }
  
  if (any(!is.finite(primaryDemands))) {
    warning("tradedPrimaries: output contains NaN/Inf values - check input data")
  }

  # ===========================================================================
  # AGGREGATE livestock dimension if !disaggLivestock
  # ===========================================================================

  if (!disaggLivestock) {
    primSecdSlice <- primaryDemands[, , primSecdDimNames]
    # Collapse kli dimension (dim 3.1) of feed entries, then rename to feed.kve
    feedAgg <- dimSums(primaryDemands[, , feedDimNames], dim = 3.1)
    getNames(feedAgg) <- paste("feed", getNames(feedAgg), sep = ".")
    primaryDemands <- mbind(primSecdSlice, feedAgg)
  }

  # ===========================================================================
  # OUTPUT
  # ===========================================================================
  
  out(primaryDemands, file)
}
