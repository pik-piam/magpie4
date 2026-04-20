#' @title tradedPrimariesBilateral
#' @description Converts trade flows into primary product equivalents by tracing
#'              secondary and livestock products back to their primary inputs.
#'              Supports both net trade flows and bilateral trade matrices.
#'              Trade flows are decomposed into three pathways:
#'              (1) direct primary trade, (2) primaries embodied in secondary products,
#'              and (3) primaries needed as feed for livestock.
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param bilateral Logical. If TRUE, uses bilateral trade matrix (ov21_trade) if available.
#'                  If FALSE, uses net trade flows. Default is TRUE.
#' @param convFactor Character. When using bilateral trade, determines whether processing
#'                     shares and feed baskets are taken from "exporter" or "importer" region.
#'                     Default is "exporter" (footprint allocation to producing region).
#' @param kastner Logical. If TRUE and bilateral=TRUE, applies Kastner et al. 2011 adjustment
#'                to bilateral trade matrix. Default is TRUE.
#' @param level Regional aggregation level ("reg", "glo", "regglo", or custom). Default is "reg".
#'
#' @details When convFactor="exporter", the exporting region's processing pathways and 
#'          feed baskets are used, which is appropriate for production-based footprint accounting.
#'          When convFactor="importer", the importing region's factors are used, which 
#'          maintains backward compatibility with the original tradedPrimaries function.
#'          
#'          For bilateral trade, the output includes full origin-destination detail.
#'          For net trade, the output is aggregated by region.
#'
#' @return MAgPIE object with primary product trade equivalents in dry matter (tDM).
#'         For bilateral trade: dimensions are (exporter.importer, year, pathway.product)
#'         For net trade: dimensions are (region, year, pathway.product)
#' @author David M Chen, Kristine Karstens
#' 
#' @importFrom madrat toolConditionalReplace
#' @importFrom gdx2 readGDX
#' @importFrom magclass getItems dimSums collapseNames mbind setNames add_columns new.magpie getYears getRegions getNames add_dimension
#' @examples
#' \dontrun{
#'   # Bilateral trade with exporter's factors (production-based accounting)
#'   x <- tradedPrimariesBilateral(gdx, bilateral = TRUE, convFactor = "exporter")
#'   
#'   # Bilateral trade with importer's factors (consumption-based accounting)
#'   x <- tradedPrimariesBilateral(gdx, bilateral = TRUE, convFactor = "importer")
#'   
#'   # Net trade (backward compatible with tradedPrimaries)
#'   x <- tradedPrimariesBilateral(gdx, bilateral = FALSE)
#' }
#'
#' @export

tradedPrimariesBilateral <- function(gdx, 
                                     file = NULL, 
                                     bilateral = TRUE,
                                     convFactor = "exporter",
                                     kastner = TRUE,
                                     level = "reg") {
  
  # Validate inputs
  if (!convFactor %in% c("exporter", "importer")) {
    stop("convFactor must be either 'exporter' or 'importer'")
  }
  
  # ===========================================================================
  # GET TRADE FLOWS
  # ===========================================================================
  
  if (bilateral) {
    # Try to read bilateral trade matrix
    tradeFlows <- tryCatch({
      tmp <- readGDXBilateral(gdx = gdx, "ov21_trade")[, , "level"]
      collapseNames(tmp)
    }, error = function(e) {
      message("Bilateral trade data (ov21_trade) not found. Falling back to net trade.")
      NULL
    })
    
    if (is.null(tradeFlows)) {
      bilateral <- FALSE
      tradeFlows <- trade(gdx, type = "net-exports")
    }
  } else {
    # Use net trade flows
    tradeFlows <- trade(gdx, type = "net-exports")
  }
  
  # ===========================================================================
  # HELPER FUNCTION: Apply regional factors to bilateral trade
  # ===========================================================================
  # This function handles the dimension renaming trick to multiply bilateral

  # trade by either exporter's or importer's regional factors
  
  .applyRegionalFactor <- function(bilateralTrade, regionalFactor, convFactor) {
    # bilateralTrade: (exporter.importer, year, product)
    # regionalFactor: (region, year, ...)
    # convFactor: "exporter" or "importer"
    
    if (convFactor == "exporter") {
      # Rename importer or exporter dimension to prevent matching, so factor applies to the correct region dim
      dim = 1.2 } else if (convFactor == "importer") {
      dim = 1.1 }
      getItems(bilateralTrade, dim = dim) <- paste0(getItems(bilateralTrade, dim = dim), "_tmp")
      result <- bilateralTrade * regionalFactor
      getItems(result, dim = dim) <- sub("_tmp$", "", getItems(result, dim = dim))
    return(result)
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
  
  # ===========================================================================
  # PROCESS TRADE FLOWS
  # ===========================================================================
  
  if (bilateral) {
    # -------------------------------------------------------------------------
    # BILATERAL TRADE PROCESSING
    # -------------------------------------------------------------------------
    
    message("Using bilateral trade matrix with ", convFactor, "'s processing/feed factors.")
    
    # Add missing items that may be needed for feed and processsing
    missingItems <- c("pasture", "foddr", "res_nonfibrous", "res_fibrous", 
                      "res_cereals", "distillers_grain", "scp", "oilpalm", "begr", "betr")
    missingItems <- setdiff(missingItems, getItems(tradeFlows, dim = 3))
    if (length(missingItems) > 0) {
      tradeFlows <- add_columns(tradeFlows, addnm = missingItems, fill = 0, dim = 3)
    }
    
    # --- PRIMARY PRODUCTS: Direct trade ---
    tradedKcrProducts <- intersect(getItems(tradeFlows, dim = 3), kcr)
    primaryDemandsPrim <- tradeFlows[, , tradedKcrProducts]
    
    # --- SECONDARY PRODUCTS: Convert to primary equivalents ---
    tradedKsdProducts <- intersect(getItems(tradeFlows, dim = 3), ksd)
    if (length(tradedKsdProducts) > 0) {
      secdTrade <- tradeFlows[, , tradedKsdProducts]
      
      # Apply conversion factors from specified region (exporter or importer)
      primaryDemandsSecd <- .applyRegionalFactor(secdTrade, 
                                                  primPerSecd[, , tradedKsdProducts], 
                                                  convFactor)
      # Sum over secondary products to get total primary demand
      primaryDemandsSecd <- dimSums(primaryDemandsSecd, dim = 3.1)
    } else {
      primaryDemandsSecd <- NULL
    }
    
    # --- LIVESTOCK PRODUCTS: Convert to feed equivalents ---
    tradedKliProducts <- intersect(getItems(tradeFlows, dim = 3), kli)
    if (length(tradedKliProducts) > 0) {
      liTrade <- tradeFlows[, , tradedKliProducts]
      # Rename to match feed basket naming
      getNames(liTrade) <- paste0("kli_", getNames(liTrade))
      
      # Account for livestock-as-feed (iterative)
      # For feed baskets, we need to handle the two-level dimension (kli_product.feed_item) 
      # differently than .applyRegionalFactor which expects single-level third dimension
      totalLiDemand <- liTrade
      
      # Determine which dimension to mask based on convFactor
      if (convFactor == "exporter") {
        maskDim <- 1.2
      } else {
        maskDim <- 1.1
      }
      
      for (iter in 1:10) {
        # Temporarily rename the non-matching spatial dimension to prevent wrong broadcasting
        tmpLiDemand <- totalLiDemand
        getItems(tmpLiDemand, dim = maskDim) <- paste0(getItems(tmpLiDemand, dim = maskDim), "_tmp")
        allFeedDemands <- tmpLiDemand * feedBaskets
        getItems(allFeedDemands, dim = maskDim) <- sub("_tmp$", "", getItems(allFeedDemands, dim = maskDim))
        
        # Extract livestock products used as feed (dim 3.2 contains feed items including kli)
        kliInFeedItems <- intersect(getItems(allFeedDemands, dim = 3.2), kli)
        if (length(kliInFeedItems) == 0) break
        liInFeed <- dimSums(allFeedDemands[, , kliInFeedItems], dim = 3.1)
        liInFeed <- setNames(liInFeed, paste0("kli_", getNames(liInFeed)))
        if (max(abs(liInFeed), na.rm = TRUE) < 1e-6) break
        totalLiDemand <- totalLiDemand + liInFeed
      }
      
      # Recalculate final feed demands with total livestock demand
      tmpLiDemand <- totalLiDemand
      getItems(tmpLiDemand, dim = maskDim) <- paste0(getItems(tmpLiDemand, dim = maskDim), "_tmp")
      feedDemands <- tmpLiDemand * feedBaskets
      getItems(feedDemands, dim = maskDim) <- sub("_tmp$", "", getItems(feedDemands, dim = maskDim))
      
      # Separate feed by type
      kveInFeed <- intersect(getItems(feedDemands, dim = 3.2), kve)
      feedDemandsPrim <- dimSums(feedDemands[, , kveInFeed], dim = 3.1)
      
      ksdInFeed <- intersect(getItems(feedDemands, dim = 3.2), ksd)
      if (length(ksdInFeed) > 0) {
        feedDemandsSecd <- feedDemands[, , ksdInFeed]
        # Convert secondary feed to primary equivalents
        feedDemandsSecd <- .applyRegionalFactor(feedDemandsSecd, 
                                                 primPerSecd[, , ksdInFeed], 
                                                 convFactor)
        feedDemandsSecd <- dimSums(feedDemandsSecd, dim = c(3.1, 3.2))
      } else {
        feedDemandsSecd <- NULL
      }
    } else {
      feedDemandsPrim <- NULL
      feedDemandsSecd <- NULL
    }
    
    # --- COMBINE ALL PATHWAYS ---
    # Get region pairs for output structure
    regionPairs <- getItems(tradeFlows, dim = 1)
    
    types <- c("prim", "secd", "feed")
    dataDimNames <- as.vector(outer(types, kve, paste, sep = "."))
    
    primaryDemands <- new.magpie(regionPairs, simYears, names = dataDimNames, fill = 0)
    
    # Populate primary pathway
    kcrInOutput <- intersect(tradedKcrProducts, kve)
    if (length(kcrInOutput) > 0) {
      primaryDemands[, , "prim"][, , kcrInOutput] <- primaryDemandsPrim[, , kcrInOutput]
    }
    
    # Populate secondary pathway
    if (!is.null(primaryDemandsSecd)) {
      kcrFromSecd <- intersect(getItems(primaryDemandsSecd, dim = 3), kve)
      if (length(kcrFromSecd) > 0) {
        primaryDemands[, , "secd"][, , kcrFromSecd] <- primaryDemandsSecd[, , kcrFromSecd]
      }
    }
    
    # Populate feed pathway
    if (!is.null(feedDemandsPrim)) {
      kveFromFeed <- intersect(getItems(feedDemandsPrim, dim = 3), kve)
      if (length(kveFromFeed) > 0) {
        primaryDemands[, , "feed"][, , kveFromFeed] <- feedDemandsPrim[, , kveFromFeed]
      }
    }
    if (!is.null(feedDemandsSecd)) {
      kcrFromFeedSecd <- intersect(getItems(feedDemandsSecd, dim = 3), kve)
      if (length(kcrFromFeedSecd) > 0) {
        primaryDemands[, , "feed"][, , kcrFromFeedSecd] <- 
          primaryDemands[, , "feed"][, , kcrFromFeedSecd] + feedDemandsSecd[, , kcrFromFeedSecd]
      }
    }
    
    # Apply Kastner adjustment if requested
    if (kastner) {
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
    
  } else {
    # -------------------------------------------------------------------------
    # NET TRADE PROCESSING (backward compatible with tradedPrimaries)
    # -------------------------------------------------------------------------
    
    message("NOTE: Using net trade flows. Processing shares and livestock feed baskets of ",
            "importing regions are used to calculate primary product requirements.")
    
    # --- PRIMARY PRODUCTS ---
    tradedKcrProducts <- intersect(getNames(tradeFlows), kcr)
    primaryDemandsPrim <- tradeFlows[, , tradedKcrProducts]
    
    # --- SECONDARY PRODUCTS ---
    tradedKsdProducts <- intersect(getNames(tradeFlows), ksd)
    secdTrade <- tradeFlows[, , tradedKsdProducts]
    primaryDemandsSecd <- secdTrade * primPerSecd[, simYears, tradedKsdProducts]
    primaryDemandsSecd <- dimSums(primaryDemandsSecd, dim = 3.1)
    
    # --- LIVESTOCK PRODUCTS ---
    liTrade <- setNames(tradeFlows[, , kli], paste0("kli_", getNames(tradeFlows[, , kli])))
    
    # Iterative livestock-as-feed calculation
    totalLiDemand <- liTrade
    for (iter in 1:10) {
      allFeedDemands <- feedBaskets[, simYears, ] * totalLiDemand
      liInFeed <- dimSums(allFeedDemands[, , paste0("kli_", kli), drop = FALSE], dim = 3.1)
      if (max(abs(liInFeed), na.rm = TRUE) < 1e-6) break
      totalLiDemand <- totalLiDemand + liInFeed
    }
    
    feedDemands <- feedBaskets[, simYears, ] * totalLiDemand
    
    # Separate feed types
    feedDemandsPrim <- dimSums(feedDemands[, , kve], dim = 3.1)
    getNames(feedDemandsPrim) <- gsub("\\..*$", "", getNames(feedDemandsPrim))
    feedDemandsPrim <- dimSums(feedDemandsPrim, dim = 3, na.rm = TRUE)
    
    feedDemandsSecd <- dimSums(feedDemands[, , ksd] * primPerSecd[, simYears, ], dim = c(3.1, 3.2))
    
    # --- COMBINE PATHWAYS ---
    types <- c("prim", "secd", "feed")
    dataDimNames <- as.vector(outer(types, kve, paste, sep = "."))
    primaryDemands <- new.magpie(getRegions(primaryDemandsPrim), simYears, 
                                  names = dataDimNames, fill = 0)
    
    kcrFromSecd <- intersect(getItems(primaryDemandsSecd, dim = 3), kve)
    kveFromFeed <- intersect(getNames(feedDemandsPrim), kve)
    kcrFromFeedSecd <- intersect(getItems(feedDemandsSecd, dim = 3), kve)
    
    primaryDemands[, , "prim"][, , tradedKcrProducts] <- primaryDemandsPrim
    primaryDemands[, , "secd"][, , kcrFromSecd] <- primaryDemandsSecd[, , kcrFromSecd]
    primaryDemands[, , "feed"][, , kveFromFeed] <- feedDemandsPrim[, , kveFromFeed]
    primaryDemands[, , "feed"][, , kcrFromFeedSecd] <- 
      primaryDemands[, , "feed"][, , kcrFromFeedSecd] + feedDemandsSecd[, , kcrFromFeedSecd]
  }
  
  # ===========================================================================
  # OUTPUT
  # ===========================================================================
  
  out(primaryDemands, file)
}
