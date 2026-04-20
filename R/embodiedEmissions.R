#' @title embodiedEmissions
#' @description Calculates production-based and consumption-based (embodied) emissions 
#' accounting using bilateral trade flows. 
#' 
#' Key insight: Different emission types require different trade treatment:
#' - Livestock-specific emissions (CH4 enteric fermentation, AWMS) are attributed to 
#'   livestock products and traded directly
#' - Feed/crop emissions (CO2 LUC, N2O fertilizers, residue burning) are attributed to 
#'   crops and traded using primary equivalents (livestock converted to feed)
#'
#' This uses the Kastner bilateral trade adjustment method.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), 
#'   "regglo" (regional and global) or any other aggregation level defined in superAggregate.
#'   Only used when bilateral=FALSE.
#' @param type Type of accounting: "production" (production-based), "consumption" 
#'   (consumption-based), "net-trade" (consumption minus production), "all" (all three),
#'   or "flows" (bilateral flows, requires bilateral=TRUE)
#' @param unit GWP metric: "GWP100AR5", "GWP100AR6", "GWP*AR5", "GWP*AR6", "gas", or "element"
#' @param pollutants Selection of pollutants: "co2", "ch4", "n2o", "nh3", "no2", "no3" or "all"
#' @param aggregation Aggregate over products ("product"), pollutants ("pollutant"), 
#'   both ("both"), or none (FALSE)
#' @param kastner Logical; apply Kastner bilateral trade adjustment (default TRUE)
#' @param bilateral Logical; if TRUE, returns bilateral flows with dimensions 
#'   (exporter.importer, year, product) instead of regional totals (default FALSE)
#'
#' @return Embodied emissions as MAgPIE object (unit depends on \code{unit}).
#'   When bilateral=FALSE: dimensions are (region, year, accounting.product).
#'   When bilateral=TRUE: dimensions are (exporter.importer, year, product).
#' @author David M Chen
#' @importFrom magclass collapseNames mbind dimSums dimOrder setNames getItems getYears add_dimension
#' @examples
#' \dontrun{
#'   x <- embodiedEmissions(gdx, type = "all", unit = "GWP100AR6")
#'   # Bilateral flows
#'   xBilat <- embodiedEmissions(gdx, type = "flows", bilateral = TRUE)
#' }
#'

embodiedEmissions <- function(gdx, 
                             file = NULL, 
                             level = "reg", 
                             type = "all",
                             unit = "GWP100AR6",
                             pollutants = "all",
                             aggregation = "pollutant",
                             kastner = TRUE,
                             bilateral = FALSE) {
  
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
  
  # ==============================================================================
  # GET PRODUCTION-BASED EMISSIONS
  # ==============================================================================
  # productEmissions allocates emissions to products:
  # - CH4 enteric fermentation -> livst_rum, livst_milk (LIVESTOCK-SPECIFIC)
  # - CH4 AWMS -> livestock types (LIVESTOCK-SPECIFIC)
  # - CH4 rice -> rice_pro (CROP)
  # - CH4 residue burning -> crops (CROP)
  # - CO2 LUC -> crops and pasture (CROP/PASTURE)
  # - N2O -> crops and livestock (BOTH - but livestock part is from manure management)
  
  # Get total emissions (for production-based accounting)
  emisProd <- productEmissions(gdx, level = "reg", unit = unit, perTonne = FALSE)
  
  # Get emissions intensity (for embodied trade calculations)
  emisIntensity <- productEmissions(gdx, level = "reg", unit = unit, perTonne = TRUE)
  
  # ==============================================================================
  # IDENTIFY LIVESTOCK VS CROP/FEED PRODUCTS
  # ==============================================================================
  
  livestockProducts <- c("livst_chick", "livst_egg", "livst_milk", "livst_pig", "livst_rum")
  emisProducts <- getItems(emisProd, dim = 3.2)
  
  # Livestock products with emissions
  livEmisProducts <- intersect(livestockProducts, emisProducts)
  # Crop/feed products (everything else)
  cropEmisProducts <- setdiff(emisProducts, livestockProducts)
  
  # ==============================================================================
  # GET BILATERAL TRADE FLOWS
  # ==============================================================================
  
  # 1. Direct trade for livestock products (Kastner-adjusted)
  tradeRaw <- tryCatch({
    collapseNames(readGDXBilateral(gdx = gdx, "ov21_trade"))
  }, error = function(e) {
    stop("Bilateral trade data not found in gdx. This function requires a MAgPIE run with bilateral trade module.")
  })
  tradeRaw <- tradeRaw[, , "level", drop = TRUE]
  if (kastner) {
    tradeLivestock <- tradeKastner(gdx = gdx, 
                                   trade = tradeRaw[, , livEmisProducts],
                                   level = "reg", 
                                   products = "kall", attributes = "dm")
  } else {
    tradeLivestock <- tradeRaw[, , livEmisProducts]
  }
  
  # 2. Primary-equivalent trade for crop/feed products
  # This converts livestock -> feed, secondary -> primary
  # Keep pathway disaggregation (prim/secd/feed) for attribution
  tradePrimary <- tradedPrimariesBilateral(gdx, kastner = kastner, level = "reg")
  # Do NOT collapse pathway dimension — tradePrimary has dim 3 = pathway.product

  # Add "direct" pathway dimension to livestock trade for consistency
  tradeLivestock <- add_dimension(tradeLivestock, dim = 3.1, add = "pathway", nm = "direct")

  # ==============================================================================
  # HELPER FUNCTION: Calculate embodied emissions for a trade matrix
  # ==============================================================================
  
  .calculateEmbodied <- function(tradeMatrix, productList, emisInt, bilateral = FALSE) {
    # tradeMatrix has dim 3 = pathway.product
    # emisInt has dim 3 = pollutant.product
    # Filter to common products (using product subdimension = dim 3.2)
    tradeProds <- unique(getItems(tradeMatrix, dim = 3.2))
    commonProds <- intersect(productList, tradeProds)
    commonProds <- intersect(commonProds, getItems(emisInt, dim = 3.2))
    
    if (length(commonProds) == 0) {
      if (bilateral) {
        return(list(bilateral = NULL))
      } else {
        return(list(export = NULL, import = NULL))
      }
    }
    
    tradeMatrix <- tradeMatrix[, , commonProds]
    emisInt <- emisInt[, , commonProds]
    
    # Apply exporter's intensity using dimension renaming trick
    # Rename importer dimension to prevent matching, so intensity applies to exporter
    # magclass broadcasts emisInt (pollutant.product) across pathway subdimension of trade
    getItems(tradeMatrix, dim = 1.2) <- paste0(getItems(tradeMatrix, dim = 1.2), "_im")
    embodiedTrade <- tradeMatrix * emisInt
    getItems(embodiedTrade, dim = 1.2) <- sub("_im$", "", getItems(embodiedTrade, dim = 1.2))
    
    if (bilateral) {
      # Return bilateral flows directly
      return(list(bilateral = embodiedTrade))
    } else {
      # Exports: sum over importers (dim 1.2)
      emisExport <- dimSums(embodiedTrade, dim = 1.2)
      
      # Imports: sum over exporters (dim 1.1)
      emisImport <- dimSums(embodiedTrade, dim = 1.1)
      
      return(list(export = emisExport, import = emisImport))
    }
  }
  
  # ==============================================================================
  # CALCULATE EMBODIED EMISSIONS FOR LIVESTOCK PRODUCTS
  # ==============================================================================
  
  livResult <- .calculateEmbodied(tradeLivestock, livEmisProducts, emisIntensity, bilateral)
  
  # ==============================================================================
  # CALCULATE EMBODIED EMISSIONS FOR CROP/FEED PRODUCTS
  # ==============================================================================
  
  # Get crop products that are in the primary trade matrix
  cropProdsInTrade <- intersect(cropEmisProducts, getItems(tradePrimary, dim = 3.2))
  cropResult <- .calculateEmbodied(tradePrimary, cropProdsInTrade, emisIntensity, bilateral)
  
  # ==============================================================================
  # COMBINE LIVESTOCK AND CROP RESULTS
  # ==============================================================================
  
  if (bilateral) {
    # Combine bilateral flows from livestock and crop results
    out <- NULL
    if (!is.null(livResult$bilateral)) {
      out <- livResult$bilateral
    }
    if (!is.null(cropResult$bilateral)) {
      if (is.null(out)) {
        out <- cropResult$bilateral
      } else {
        out <- mbind(out, cropResult$bilateral)
      }
    }
    
    # Apply pollutant filter if specified
    if (pollutants != "all") {
      availablePollutants <- getItems(out, dim = "pollutants")
      requestedPollutants <- intersect(pollutants, availablePollutants)
      if (length(requestedPollutants) == 0) {
        stop(paste("No matching pollutants found. Available:", paste(availablePollutants, collapse = ", ")))
      }
      out <- out[, , requestedPollutants]
    }
    
    # Apply aggregation if requested
    if (aggregation == "product") {
      out <- dimSums(out, dim = "k")
    } else if (aggregation == "pollutant") {
      if (!grepl("GWP", unit)) {
        warning("Aggregating pollutants only makes sense for GWP units. ",
                "With unit='", unit, "', pollutants have different units.")
      }
      out <- dimSums(out, dim = "pollutants")
    } else if (aggregation == "both") {
      if (!grepl("GWP", unit)) {
        warning("Aggregating pollutants only makes sense for GWP units. ",
                "With unit='", unit, "', pollutants have different units.")
      }
      out <- dimSums(out, dim = c("k", "pollutants"))
    }
    
    # Write to file if requested
    if (!is.null(file)) {
      write.magpie(out, file_name = file)
    }
    
    return(out)
  }
  
  # ==============================================================================
  # NON-BILATERAL: Build production, export, import with pathway disaggregation
  # Output retains pathway dimension (production/direct/prim/secd/feed)
  # ==============================================================================
  
  # No pathway dimension on production — pathway only applies to trade flows

  # Initialize export/import — these come from .calculateEmbodied with pathway dims
  emisExportComponents <- list()
  emisImportComponents <- list()
  
  # Add livestock results (pathway = "direct")
  if (!is.null(livResult$export)) {
    emisExportComponents[["liv"]] <- livResult$export
    emisImportComponents[["liv"]] <- livResult$import
  }
  
  # Add crop results (pathway = prim/secd/feed)
  if (!is.null(cropResult$export)) {
    emisExportComponents[["crop"]] <- cropResult$export
    emisImportComponents[["crop"]] <- cropResult$import
  }
  
  emisExport <- if (length(emisExportComponents) > 0) mbind(emisExportComponents) else NULL
  emisImport <- if (length(emisImportComponents) > 0) mbind(emisImportComponents) else NULL



  # ==============================================================================
  # CALCULATE CONSUMPTION-BASED EMISSIONS
  # ==============================================================================
  if (!is.null(emisExport) && !is.null(emisImport)) {
    # Net trade keeps pathway dimension (direct/prim/secd/feed)
    emisNetTrade <- emisImport - emisExport
    # Consumption = production + net trade (collapse pathway for clean product-level result)
    emisConsumption <- emisProd + dimSums(emisNetTrade, dim = 3.1)
  } else {
    emisNetTrade <- emisProd * 0
    emisConsumption <- emisProd
  }


  # ==============================================================================
  # FILTER BY POLLUTANTS IF SPECIFIED
  # ==============================================================================
  
  if (pollutants != "all") {
    availablePollutants <- unique(getItems(emisConsumption, dim = "pollutants"))
    requestedPollutants <- intersect(pollutants, availablePollutants)
    if (length(requestedPollutants) == 0) {
      stop(paste("No matching pollutants found. Available:", paste(availablePollutants, collapse = ", ")))
    }
    emisProd <- emisProd[, , requestedPollutants]
    emisConsumption <- emisConsumption[, , requestedPollutants]
    emisNetTrade <- emisNetTrade[, , requestedPollutants]
  }
  
  # ==============================================================================
  # APPLY AGGREGATION IF REQUESTED
  # ==============================================================================
  
  if (aggregation == "product") {
    emisProd <- dimSums(emisProd, dim = "k")
    emisConsumption <- dimSums(emisConsumption, dim = "k")
    emisNetTrade <- dimSums(emisNetTrade, dim = "k")
  } else if (aggregation == "pollutant") {
    # Only makes sense for GWP units where all pollutants are in CO2eq
    if (!grepl("GWP", unit)) {
      warning("Aggregating pollutants only makes sense for GWP units. ",
              "With unit='", unit, "', pollutants have different units.")
    }
    emisProd <- dimSums(emisProd, dim = "pollutants")
    emisConsumption <- dimSums(emisConsumption, dim = "pollutants")
    emisNetTrade <- dimSums(emisNetTrade, dim = "pollutants")
  } else if (aggregation == "both") {
    if (!grepl("GWP", unit)) {
      warning("Aggregating pollutants only makes sense for GWP units. ",
              "With unit='", unit, "', pollutants have different units.")
    }
    emisProd <- dimSums(emisProd, dim = c("k", "pollutants"))
    emisConsumption <- dimSums(emisConsumption, dim = c("k", "pollutants"))
    emisNetTrade <- dimSums(emisNetTrade, dim = c("k", "pollutants"))
  }
  
  # ==============================================================================
  # PREPARE OUTPUT BASED ON REQUESTED TYPE
  # dim 3 structure: accounting.pathway.[pollutant].[product]
  # ==============================================================================

  if (type == "production") {
    out <- add_dimension(emisProd, dim = 3.1, add = "accounting", nm = "production")
  } else if (type == "consumption") {
    out <- add_dimension(emisConsumption, dim = 3.1, add = "accounting", nm = "consumption")
  } else if (type == "net-trade") {
    out <- add_dimension(emisNetTrade, dim = 3.1, add = "accounting", nm = "net-trade")
  } else if (type == "all") {
    out <- mbind(
      add_dimension(emisProd, dim = 3.1, add = "accounting", nm = "production"),
      add_dimension(emisConsumption, dim = 3.1, add = "accounting", nm = "consumption"),
      add_dimension(dimSums(emisNetTrade, dim = 3.1), dim = 3.1, add = "accounting", nm = "net-trade")
    )
  } else {
    stop("Invalid type. Choose from: 'production', 'consumption', 'net-trade', or 'all'")
  }
  
  # Apply regional aggregation if requested
  if (level != "reg") {
    out <- superAggregate(out, aggr_type = "sum", level = level, na.rm = TRUE)
  }
  
  # Write to file if requested
  if (!is.null(file)) {
    write.magpie(out, file_name = file)
  }
  
  return(out)
}
