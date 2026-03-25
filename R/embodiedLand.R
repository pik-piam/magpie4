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
#'   (consumption-based), "net-trade" (consumption minus production), "all" (all three),
#'   or "flows" (bilateral flows, requires bilateral=TRUE)
#' @param landType Type of land to report: "crop" (cropland), "past" (pasture), 
#'   "all" (total agricultural land), or a vector of specific land types
#' @param bilateral Logical; if TRUE, returns bilateral flows with dimensions 
#'   (exporter.importer, year, product) instead of regional totals (default FALSE)
#'
#' @return Embodied land use as MAgPIE object.
#'   When bilateral=FALSE: dimensions are (region, year, accounting.product).
#'   When bilateral=TRUE: dimensions are (exporter.importer, year, product).
#' @author David M Chen
#' @seealso \code{\link{land}}, \code{\link{croparea}}, \code{\link{trade}}
#' @importFrom magclass collapseNames mbind dimSums dimOrder setNames getItems getYears add_dimension
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
                                         kastner = TRUE, level = level)
  
  # Keep pathway disaggregation (prim/secd/feed) — do NOT collapse with dimSums
  # This allows later attribution of feed component back to livestock
  
  # Common products between trade and land intensity
  # trade dim 3 is pathway.product; landIntensity dim 3 is product
  tradeProducts <- unique(getItems(trade, dim = 3.2))
  commonProducts <- intersect(tradeProducts, getItems(landIntensity, dim = 3))
  
  
  trade <- trade[, , commonProducts]
  landIntensity <- landIntensity[, , commonProducts]
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
  # Keeps pathway dimension (prim/secd/feed) for attribution
  landNetTrade <- landImport - landExport
  
  # Consumption-based = production + net trade (collapse pathway for clean product-level result)
  landConsumption <- landProd + dimSums(landNetTrade, dim = 3.1)
    
  # Filter by land type if specified
  if (landType != "all") {
    if (landType == "crop") {
      cropProducts <- intersect(getItems(cropLand, dim = 3), getItems(landProd, dim = 3))
      landProd <- landProd[, , cropProducts]
      landConsumption <- landConsumption[, , cropProducts]
      landNetTrade <- landNetTrade[, , cropProducts]
    } else if (landType == "past") {
      landProd <- landProd[, , "pasture"]
      landConsumption <- landConsumption[, , "pasture"]
      landNetTrade <- landNetTrade[, , "pasture"]
    } else {
      stop("Invalid landType. Choose from: 'crop', 'past', or 'all'")
    }
  }
  
  # Prepare output based on requested type
  # Production/consumption: dim 3 = accounting.product
  # Net-trade: dim 3 = accounting.pathway.product (preserves prim/secd/feed)
  # All: dim 3 = accounting.product (pathway collapsed for consistency)
  if (type == "production") {
    out <- add_dimension(landProd, dim = 3.1, add = "accounting", nm = "production")
  } else if (type == "consumption") {
    out <- add_dimension(landConsumption, dim = 3.1, add = "accounting", nm = "consumption")
  } else if (type == "net-trade") {
    out <- add_dimension(landNetTrade, dim = 3.1, add = "accounting", nm = "net-trade")
  } else if (type == "all") {
    out <- mbind(
      add_dimension(landProd, dim = 3.1, add = "accounting", nm = "production"),
      add_dimension(landConsumption, dim = 3.1, add = "accounting", nm = "consumption"),
      add_dimension(dimSums(landNetTrade, dim = 3.1), dim = 3.1, add = "accounting", nm = "net-trade")
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
