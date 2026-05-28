#' @title embodiedLand
#' @description Calculates production-based and consumption-based (embodied) land footprint 
#' accounting using bilateral trade flows. Land use is allocated to traded products
#' based on production ratios and bilateral trade patterns.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), 
#'   "regglo" (regional and global) or any other aggregation level defined in superAggregate.
#'   Only used when bilateral=FALSE.
#' @param type Type of accounting: "production" (production-based), "consumption" 
#'   (consumption-based), "trade" (export, import, and net-trade), "all" (all five),
#'   or "flows" (bilateral flows, requires bilateral=TRUE)
#' @param landType Type of land to report: "crop" (cropland), "past" (pasture), 
#'   "all" (total agricultural land), or a vector of specific land types
#' @param bilateral Logical; if TRUE, returns bilateral flows with dimensions 
#'   (exporter.importer, year, product) instead of regional totals (default FALSE)
#' @param disaggLivestock Logical; if TRUE, the feed pathway retains the livestock product
#'   dimension, so land is attributed per animal product × feed crop combination.
#'   Passes \code{disaggLivestock} to \code{tradedPrimariesBilateral}.
#'   Use \code{dimSums(x[kli_items], dim=3.1)} to collapse to feed crops, or
#'   \code{dimSums(x[kli_items], dim=3.2)} to collapse to animal products.
#'   Default is FALSE (current behaviour: feed attributed to crops).
#'
#' @return Embodied land use as MAgPIE object.
#'   When bilateral=FALSE and disaggLivestock=FALSE: dim 3 = accounting.product (2 subdims).
#'   When bilateral=FALSE and disaggLivestock=TRUE: dim 3 = accounting.{prim,secd,kli_*}.product
#'   (3 subdims); production/consumption have prim = crop+pasture land and kli_* = feed
#'   chain land per animal product (secd=0 in production); trade types retain the full
#'   secd pathway. Note: prim and kli_* items overlap (feed crops appear in both), so
#'   they should not be summed — use one or the other for attribution.
#'   When bilateral=TRUE: dim 3 = {prim,secd,kli_*}.product (pathway.product).
#' @author David M Chen
#' @seealso \code{\link{land}}, \code{\link{croparea}}, \code{\link{trade}}
#' @importFrom magclass collapseNames mbind dimSums dimOrder setNames getNames getItems getYears add_dimension getRegions new.magpie
#' @importFrom gdx2 readGDX
#' @source tradSecondaryToPrimary.R
#' @examples
#' \dontrun{
#'   x <- embodiedLand(gdx, type = "all", landType = "all")
#'   # Bilateral flows
#'   xBilat <- embodiedLand(gdx, type = "flows", bilateral = TRUE)
#' }
#'

embodiedLand <- function(gdx,
                        file = NULL,
                        level = "reg",
                        type = "all",
                        landType = "all",
                        bilateral = FALSE,
                        disaggLivestock = FALSE) {
  
  # ==============================================================================
  # VALIDATE BILATERAL PARAMETERS
  # ==============================================================================
  
  if (bilateral) {
    if (type != "flows") {
      stop("When bilateral=TRUE, type must be 'flows'. ",
           "Production/consumption/net-trade accounting requires regional aggregation.")
    }
    if (level != "reg") {
      stop("When bilateral=TRUE, level must be 'reg'. ",
           "Regional aggregation is not supported for bilateral output.")
    }
  } else {
    if (type == "flows") {
      stop("type='flows' requires bilateral=TRUE.")
    }
  }
  
  # Get production data (for production-based accounting)
  prod <- production(gdx, level = level, product_aggr = FALSE, attributes = "dm")
  # prod has "pasture" — keep as-is to match trade (kve set uses "pasture")

  # Get land use data by product
  # Cropland
  cropLand <- croparea(gdx, level = level, products = "kcr", product_aggr = FALSE,
                       water_aggr = TRUE)

  # Pasture land
  pastLand <- land(gdx, level = level, types = "past", subcategories = FALSE)
  # Rename "past" -> "pasture" to match production and trade naming (kve set)
  getItems(pastLand, dim = 3) <- "pasture"
  cropPastLand <- mbind(cropLand, pastLand)
  
  # Calculate land use intensity (land per unit production) at regional level
  # For crops: Mha per Mt dm - THIS IS THE EXPORTER'S YIELD
  citems <- intersect(getItems(cropPastLand, dim = 3), getItems(prod, dim = 3))
  landIntensity <- cropPastLand[, , citems] / prod[, , citems]
  landIntensity[is.na(landIntensity)] <- 0
  landIntensity[is.infinite(landIntensity)] <- 0

  # Get bilateral trade converted to primary commodity equivalents
  # (livestock -> feed, secondary -> primary)
  # Output has dim 3 = pathway.product (pathway = prim/secd/feed)
  trade <- tradedPrimariesBilateral(gdx, bilateral = TRUE, convFactor = "exporter",
                                         kastner = TRUE, level = level,
                                         disaggLivestock = disaggLivestock)
  
  # Keep pathway disaggregation (prim/secd/feed) — do NOT collapse with dimSums
  # This allows later attribution of feed component back to livestock
  
  # Common products between trade and land intensity
  # trade dim 3 is pathway.product; landIntensity dim 3 is product
  tradeProducts <- unique(getItems(trade, dim = 3.2))
  commonProducts <- intersect(tradeProducts, getItems(landIntensity, dim = 3))
  
  
  trade <- trade[, , commonProducts]
  landIntensity <- landIntensity[, , commonProducts]
  prodAll <- prod  # preserve full production (including livestock) for feed basket calc
  prod <- prod[, , commonProducts]

  # Calculate production-based land footprint (total land used for production)
  # No pathway dimension — pathway only applies to trade flows
  landProd <- prod * landIntensity
 
  # Calculate embodied land in bilateral trade flows
  # Key: multiply trade flows by EXPORTER's land intensity
  # trade has dims: (exporter.importer, year, pathway.product)
  # landIntensity has dims: (region, year, product)
  # magclass broadcasts intensity across pathway subdimension
  
  # Rename importer dimension temporarily to allow multiplication by exporter only
  getItems(trade, dim = 1.2) <- paste0(getItems(trade, dim = 1.2), "_im")
  landTraded <- trade * landIntensity  # landIntensity applies to exporter (dim 1.1)
  getItems(landTraded, dim = 1.2) <- sub("_im$", "", getItems(landTraded, dim = 1.2))
  
  # ==============================================================================
  # BILATERAL OUTPUT: Return bilateral flows directly (with pathway.product dims)
  # ==============================================================================
  
  if (bilateral) {
    out <- landTraded
    
    # Filter by land type if specified
    if (landType != "all") {
      if (landType == "crop") {
        cropProducts <- intersect(getItems(cropLand, dim = 3), getItems(out, dim = 3.2))
        out <- out[, , cropProducts]
      } else if (landType == "past") {
        if ("pasture" %in% getItems(out, dim = 3)) {
          out <- out[, , "pasture"]
        } else {
          stop("Pasture not found in bilateral trade flows.")
        }
      } else {
        stop("Invalid landType. Choose from: 'crop', 'past', or 'all'")
      }
    }
    
    # Write to file if requested
    if (!is.null(file)) {
      write.magpie(out, file_name = file)
    }
    
    return(out)
  }
  
  # ==============================================================================
  # NON-BILATERAL: Calculate exports and imports of embodied land
  # Output retains pathway.product structure (pathway = production/prim/secd/feed)
  # ==============================================================================
  
  # Exports: sum over importing regions (dim 1.2 = destination)
  # Result: (region, year, pathway.product) 
  landExport <- dimSums(landTraded, dim = 1.2)
  
  # Imports: sum over exporting regions (dim 1.1 = origin)
  landImport <- dimSums(landTraded, dim = 1.1)
  
  # Net trade = imports - exports (same pathway dims, so arithmetic works)
  # Keeps pathway dimension (prim/secd/feed or prim/secd/kli_*) for attribution
  landNetTrade <- landImport - landExport

  if (disaggLivestock) {
    # =========================================================================
    # LIVESTOCK PRODUCTION LAND VIA FEED BASKETS
    # Mirrors the iterative feed loop in tradedPrimariesBilateral, but for
    # production flows (not trade flows), so domestic livestock production land
    # is attributed to specific animal products.
    # =========================================================================
    simYears <- getYears(prod)
    kli  <- readGDX(gdx, "kli")
    ksd  <- readGDX(gdx, "ksd")

    feedBaskets <- readGDX(gdx, "im_feed_baskets")
    getNames(feedBaskets, dim = "kap") <- paste0("kli_", getNames(feedBaskets, dim = "kap"))
    feedBaskets <- feedBaskets[, simYears, paste0("kli_", kli)]

    primPerSecdProd <- primaryPerSecondary(gdx, level = level, allocation = "value")
    primPerSecdProd <- primPerSecdProd[, simYears, ]

    # Livestock production in DM (rename to match feed basket naming)
    kliInProd <- intersect(getItems(prodAll, dim = 3), kli)
    liProd_dm <- prodAll[, simYears, kliInProd]
    getNames(liProd_dm) <- paste0("kli_", getNames(liProd_dm))

    # Iterative expansion for livestock used as feed
    totalLiDemand <- liProd_dm
    for (iter in 1:10) {
      allFeedDemandsProd <- feedBaskets[, simYears, ] * totalLiDemand
      kliInFeedItemsProd <- intersect(getItems(allFeedDemandsProd, dim = 3.2), kli)
      if (length(kliInFeedItemsProd) == 0) break
      liInFeedProd <- dimSums(allFeedDemandsProd[, , kliInFeedItemsProd], dim = 3.1)
      liInFeedProd <- setNames(liInFeedProd, paste0("kli_", getNames(liInFeedProd)))
      if (max(abs(liInFeedProd), na.rm = TRUE) < 1e-6) break
      totalLiDemand <- totalLiDemand + liInFeedProd
    }
    feedDemandProd <- feedBaskets[, simYears, ] * totalLiDemand

    # Primary feed crops → land (dim 3: kli_product.kve; broadcasts over kli dim)
    kveInFeedProd <- intersect(getItems(feedDemandProd, dim = 3.2), getItems(landIntensity, dim = 3))
    liProdKveLand <- feedDemandProd[, , kveInFeedProd] * landIntensity[, , kveInFeedProd]

    # Secondary feed → primary equivalent → land
    ksdInFeedProd <- intersect(getItems(feedDemandProd, dim = 3.2), ksd)
    if (length(ksdInFeedProd) > 0) {
      # feedDemandProd[,,ksd] has kli.ksd; * primPerSecd (ksd.kve) → kli.ksd.kve
      # dimSums(dim=3.2) collapses ksd → kli.kve
      feedProdSecdPrim <- dimSums(
        feedDemandProd[, , ksdInFeedProd] * primPerSecdProd[, simYears, ksdInFeedProd],
        dim = 3.2)
      kveFromSecdProd <- intersect(getItems(feedProdSecdPrim, dim = 3.2), getItems(landIntensity, dim = 3))
      if (length(kveFromSecdProd) > 0) {
        liProdKveLand[, , kveFromSecdProd] <- liProdKveLand[, , kveFromSecdProd] +
          feedProdSecdPrim[, , kveFromSecdProd] * landIntensity[, , kveFromSecdProd]
      }
    }

    # Total feed land by crop (sum over all livestock products → kve)
    feedLandTotal <- dimSums(liProdKveLand, dim = 3.1)

    # Non-feed production land = total land − land attributed to feed
    # (makes prim and kli_* sub-dimensions mutually exclusive)
    landProdNonFeed <- landProd
    commonKve <- intersect(getItems(landProd, dim = 3), getItems(feedLandTotal, dim = 3))
    if (length(commonKve) > 0) {
      landProdNonFeed[, , commonKve] <- landProd[, , commonKve] - feedLandTotal[, , commonKve]
    }

    # Build landProd with {prim, secd=0, kli_*}.kve structure (matches landNetTrade)
    # so that production + net-trade addition is dimension-aligned.
    tradeStructureNames <- getItems(landNetTrade, dim = 3)
    landProdFull <- new.magpie(getRegions(landProd), getYears(landProd),
                               names = tradeStructureNames, fill = 0)

    # Fill prim items from non-feed crop+pasture production land
    primKveAvail <- intersect(
      sub("^prim\\.", "", grep("^prim\\.", tradeStructureNames, value = TRUE)),
      getItems(landProdNonFeed, dim = 3))
    if (length(primKveAvail) > 0) {
      landProdFull[, , paste0("prim.", primKveAvail)] <- landProdNonFeed[, , primKveAvail]
    }

    # Fill kli_* items from livestock feed production land
    kliStructureItems <- grep("^kli_", tradeStructureNames, value = TRUE)
    availKliItems <- intersect(kliStructureItems, getItems(liProdKveLand, dim = 3))
    if (length(availKliItems) > 0) {
      landProdFull[, , availKliItems] <- liProdKveLand[, , availKliItems]
    }

    landProd <- landProdFull

    # Consumption = production + net trade (same {prim,secd,kli_*}.kve structure)
    landConsumption <- landProd + landNetTrade

  } else {
    # Consumption-based = production + net trade (collapse pathway for clean product-level result)
    landConsumption <- landProd + dimSums(landNetTrade, dim = 3.1)
  }
    
  # Filter by land type if specified
  if (landType != "all") {
    if (landType == "crop") {
      cropProducts <- intersect(getItems(cropLand, dim = 3), getItems(landProd, dim = 3))
      landProd <- landProd[, , cropProducts]
      landConsumption <- landConsumption[, , cropProducts]
      landExport <- landExport[, , cropProducts]
      landImport <- landImport[, , cropProducts]
      landNetTrade <- landNetTrade[, , cropProducts]
    } else if (landType == "past") {
      landProd <- landProd[, , "pasture"]
      landConsumption <- landConsumption[, , "pasture"]
      landExport <- landExport[, , "pasture"]
      landImport <- landImport[, , "pasture"]
      landNetTrade <- landNetTrade[, , "pasture"]
    } else {
      stop("Invalid landType. Choose from: 'crop', 'past', or 'all'")
    }
  }
  
  # Prepare output based on requested type.
  # disaggLivestock=FALSE: dim 3 = accounting.product (2 subdims, pathway collapsed)
  # disaggLivestock=TRUE:  dim 3 = accounting.{prim,secd,kli_*}.product (3 subdims)
  #   - production/consumption: prim = crop+pasture land, kli_* = feed chain land per animal product
  #   - trade/all: full pathway including secd
  if (type == "production") {
    out <- add_dimension(landProd, dim = 3.1, add = "accounting", nm = "production")
  } else if (type == "consumption") {
    out <- add_dimension(landConsumption, dim = 3.1, add = "accounting", nm = "consumption")
  } else if (type == "trade") {
    if (disaggLivestock) {
      out <- mbind(
        add_dimension(landExport,   dim = 3.1, add = "accounting", nm = "export"),
        add_dimension(landImport,   dim = 3.1, add = "accounting", nm = "import"),
        add_dimension(landNetTrade, dim = 3.1, add = "accounting", nm = "net-trade")
      )
    } else {
      out <- mbind(
        add_dimension(dimSums(landExport,   dim = 3.1), dim = 3.1, add = "accounting", nm = "export"),
        add_dimension(dimSums(landImport,   dim = 3.1), dim = 3.1, add = "accounting", nm = "import"),
        add_dimension(dimSums(landNetTrade, dim = 3.1), dim = 3.1, add = "accounting", nm = "net-trade")
      )
    }
  } else if (type == "all") {
    if (disaggLivestock) {
      # All five accounting types share {prim,secd,kli_*}.kve structure — no collapsing needed
      out <- mbind(
        add_dimension(landProd,        dim = 3.1, add = "accounting", nm = "production"),
        add_dimension(landConsumption, dim = 3.1, add = "accounting", nm = "consumption"),
        add_dimension(landExport,      dim = 3.1, add = "accounting", nm = "export"),
        add_dimension(landImport,      dim = 3.1, add = "accounting", nm = "import"),
        add_dimension(landNetTrade,    dim = 3.1, add = "accounting", nm = "net-trade")
      )
    } else {
      out <- mbind(
        add_dimension(landProd,                          dim = 3.1, add = "accounting", nm = "production"),
        add_dimension(landConsumption,                   dim = 3.1, add = "accounting", nm = "consumption"),
        add_dimension(dimSums(landExport,   dim = 3.1), dim = 3.1, add = "accounting", nm = "export"),
        add_dimension(dimSums(landImport,   dim = 3.1), dim = 3.1, add = "accounting", nm = "import"),
        add_dimension(dimSums(landNetTrade, dim = 3.1), dim = 3.1, add = "accounting", nm = "net-trade")
      )
    }
  } else {
    stop("Invalid type. Choose from: 'production', 'consumption', 'trade', or 'all'")
  }
  
  # Apply regional aggregation if requested
  if (level != "reg") {
    out <- superAggregate(out, aggr_type = level, na.rm = TRUE)
  }
  
  # Write to file if requested
  if (!is.null(file)) {
    write.magpie(out, file_name = file)
  }
  
  return(out)
}
