#' @title embodiedWater
#' @description Calculates production-based and consumption-based (embodied) water footprint 
#' accounting using bilateral trade flows. Water use is allocated to traded products
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
#'   (consumption-based), "net-trade" (consumption minus production), "all" (all three),
#'   or "flows" (bilateral flows, requires bilateral=TRUE)
#' @param waterType Type of water to report: "withdrawal" (water withdrawal), 
#'   "consumption" (consumptive water use)
#' @param bilateral Logical; if TRUE, returns bilateral flows with dimensions 
#'   (exporter.importer, year, product) instead of regional totals (default FALSE)
#'
#' @return Embodied water use as MAgPIE object.
#'   When bilateral=FALSE: dimensions are (region, year, accounting.product).
#'   When bilateral=TRUE: dimensions are (exporter.importer, year, product).
#' @author David M Chen
#' @seealso \code{\link{water_usage}}, \code{\link{trade}}
#' @importFrom magclass collapseNames mbind dimSums dimOrder setNames getItems getYears add_dimension
#' @importFrom magpie4 production water_usage
#' @examples
#' \dontrun{
#'   x <- embodiedWater(gdx, type = "all", waterType = "consumption")
#'   # Bilateral flows
#'   xBilat <- embodiedWater(gdx, type = "flows", bilateral = TRUE)
#' }
#'

embodiedWater <- function(gdx,
                         file = NULL, 
                         level = "reg", 
                         type = "all",
                         waterType = "consumption",
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
                        
                        
  # Get production data
  prod <- production(gdx, level = level, product_aggr = FALSE, attributes = "dm")
  # prod has "pasture" — keep as-is to match kve set / trade naming
  
  
  # Get water usage data
     waterUse <- water_usage(gdx, level = level, users = "kcr", 
                           sum = FALSE, digits = 10, abstractiontype = waterType)
    waterUsekli <- water_usage(gdx, level = level, users = "kli", 
                           sum = FALSE, digits = 10, abstractiontype = waterType)
    waterUse <- mbind(waterUse, waterUsekli)
  # Calculate water use intensity (water per unit production) - EXPORTER'S INTENSITY

    commonProducts <- intersect(getItems(prod, dim = 3), getItems(waterUse, dim = 3))
    waterIntensity <- waterUse[, , commonProducts] / prod[, , commonProducts]
    waterIntensity[is.na(waterIntensity)] <- 0
    waterIntensity[is.infinite(waterIntensity)] <- 0

  
  # Get bilateral trade flows converted to primary equivalents
  # For crops: use primary equivalents (livestock converted to feed)
  # Keep pathway disaggregation (prim/secd/feed) for attribution
  tradePrimary <- tradedPrimariesBilateral(gdx, kastner = TRUE, level = level)
  # Do NOT collapse pathway dimension
  
  # For livestock: use direct trade (Kastner-adjusted) since livestock water use
  # is attributed to where the livestock were produced, not where feed was grown
  tradeRaw <- tryCatch({
    collapseNames(readGDXBilateral(gdx = gdx, "ov21_trade") )
  }, error = function(e) {
    stop("Bilateral trade data not found in gdx. This function requires a MAgPIE run with bilateral trade module.")
  })
  
  tradeRaw <- tradeRaw[, , "level", drop = TRUE]
  tradeLivestock <- tradeKastner(gdx = gdx, trade = tradeRaw, level = level, 
                                  products = "kall", attributes = "dm")
  # Add "direct" pathway dimension to livestock trade
  tradeLivestock <- add_dimension(tradeLivestock, dim = 3.1, add = "pathway", nm = "direct")
  
  # Identify livestock vs crop products
  kli <- readGDX(gdx, "kli")
  livProducts <- intersect(kli, commonProducts)
  cropProducts <- setdiff(commonProducts, kli)

  # Update commonProducts to include only those in trade (using product subdimension)
  cropProductsInTrade <- intersect(cropProducts, unique(getItems(tradePrimary, dim = 3.2)))
  livProductsInTrade <- intersect(livProducts, unique(getItems(tradeLivestock, dim = 3.2)))
  commonProducts <- c(cropProductsInTrade, livProductsInTrade)
  
  prod <- prod[, , commonProducts]
  
  # Subset to common products
  waterIntensity <- waterIntensity[, , commonProducts]
  
  # Calculate production-based water footprint — no pathway dimension
  waterProd <- prod[, , commonProducts] * waterIntensity
  
  # ==============================================================================
  # CALCULATE EMBODIED WATER IN TRADE
  # Trade has dim 3 = pathway.product; waterIntensity has dim 3 = product
  # magclass broadcasts intensity across pathway subdimension
  # ==============================================================================
  
  # Helper function to calculate embodied water for a trade matrix
  .calcEmbodiedWater <- function(tradeMatrix, products, intensity, bilateral = FALSE) {
    if (length(products) == 0) {
      if (bilateral) {
        return(list(bilateral = NULL))
      } else {
        return(list(export = NULL, import = NULL))
      }
    }
    
    tradeMatrix <- tradeMatrix[, , products]
    intensity <- intensity[, , products]
    
    # Rename importer dimension temporarily to allow multiplication by exporter only
    getItems(tradeMatrix, dim = 1.2) <- paste0(getItems(tradeMatrix, dim = 1.2), "_im")
    waterTraded <- tradeMatrix * intensity
    getItems(waterTraded, dim = 1.2) <- sub("_im$", "", getItems(waterTraded, dim = 1.2))
    
    if (bilateral) {
      # Return bilateral flows directly (with pathway.product dims)
      return(list(bilateral = waterTraded))
    } else {
      # Exports: sum over importing regions
      waterExp <- dimSums(waterTraded, dim = 1.2)
      # Imports: sum over exporting regions
      waterImp <- dimSums(waterTraded, dim = 1.1)
      
      return(list(export = waterExp, import = waterImp))
    }
  }
  
  # Calculate for crops (using primary equivalents trade — has prim/secd/feed pathways)
  cropResult <- .calcEmbodiedWater(tradePrimary, cropProductsInTrade, waterIntensity, bilateral)
  
  # Calculate for livestock (using direct trade — has "direct" pathway)
  livResult <- .calcEmbodiedWater(tradeLivestock, livProductsInTrade, waterIntensity, bilateral)
  
  # ==============================================================================
  # COMBINE RESULTS
  # ==============================================================================
  
  if (bilateral) {
    # Combine bilateral flows from livestock and crop results (with pathway dims)
    out <- NULL
    if (!is.null(cropResult$bilateral)) {
      out <- cropResult$bilateral
    }
    if (!is.null(livResult$bilateral)) {
      if (is.null(out)) {
        out <- livResult$bilateral
      } else {
        out <- mbind(out, livResult$bilateral)
      }
    }
    
    # Write to file if requested
    if (!is.null(file)) {
      write.magpie(out, file_name = file)
    }
    
    return(out)
  }
  
  # ==============================================================================
  # NON-BILATERAL: Combine results with pathway disaggregation
  # Output retains pathway dimension (production/direct/prim/secd/feed)
  # ==============================================================================
  
  waterExportComponents <- list()
  waterImportComponents <- list()
  
  if (!is.null(cropResult$export)) {
    waterExportComponents[["crop"]] <- cropResult$export
    waterImportComponents[["crop"]] <- cropResult$import
  }
  if (!is.null(livResult$export)) {
    waterExportComponents[["liv"]] <- livResult$export
    waterImportComponents[["liv"]] <- livResult$import
  }
  
  waterExport <- if (length(waterExportComponents) > 0) mbind(waterExportComponents) else NULL
  waterImport <- if (length(waterImportComponents) > 0) mbind(waterImportComponents) else NULL
  
  # Net trade = imports - exports (keeps pathway: direct/prim/secd/feed)
  if (!is.null(waterExport) && !is.null(waterImport)) {
    waterNetTrade <- waterImport - waterExport
    waterConsump <- waterProd + dimSums(waterNetTrade, dim = 3.1)
  } else {
    waterNetTrade <- waterProd * 0
    waterConsump <- waterProd
  }

  # Prepare output based on requested type
  # Production/consumption: dim 3 = accounting.product
  # Net-trade: dim 3 = accounting.pathway.product (preserves prim/secd/feed/direct)
  # All: dim 3 = accounting.product (pathway collapsed for consistency)
  if (type == "production") {
    out <- add_dimension(waterProd, dim = 3.1, add = "accounting", nm = "production")
  } else if (type == "consumption") {
    out <- add_dimension(waterConsump, dim = 3.1, add = "accounting", nm = "consumption")
  } else if (type == "net-trade") {
    out <- add_dimension(waterNetTrade, dim = 3.1, add = "accounting", nm = "net-trade")
  } else if (type == "all") {
    out <- mbind(
      add_dimension(waterProd, dim = 3.1, add = "accounting", nm = "production"),
      add_dimension(waterConsump, dim = 3.1, add = "accounting", nm = "consumption"),
      add_dimension(dimSums(waterNetTrade, dim = 3.1), dim = 3.1, add = "accounting", nm = "net-trade")
    )
  } else {
    stop("Invalid type. Choose from: 'production', 'consumption', 'net-trade', or 'all'")
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
