#' @title embodiedBiodiversity
#' @description Calculates production-based and consumption-based (embodied) biodiversity 
#' impact accounting using bilateral trade flows. Biodiversity values (BII-weighted area)
#' are allocated to traded products based on cropland and pasture area shares, 
#' similar to how CO2 LUC emissions are allocated.
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
#' @param indicator Which biodiversity indicator to use: "bv" (biodiversity value, 
#'   BII-weighted area in Mha), "bii_loss" (1-BII, representing biodiversity loss)
#' @param bilateral Logical; if TRUE, returns bilateral flows with dimensions 
#'   (exporter.importer, year, product) instead of regional totals (default FALSE)
#'
#' @return Embodied biodiversity impact as MAgPIE object.
#'   When bilateral=FALSE: dimensions are (region, year, accounting.product).
#'   When bilateral=TRUE: dimensions are (exporter.importer, year, product).
#' @author David M Chen
#' @seealso \code{\link{BII}}, \code{\link{land}}, \code{\link{croparea}}, \code{\link{trade}}
#' @importFrom magclass collapseNames mbind dimSums dimOrder setNames getItems getYears add_dimension
#' @importFrom magpie4 production croparea land BII
#' @details 
#' Biodiversity is measured via the Biodiversity Intactness Index (BII) which ranges from 0 to 1.
#' BII is calculated at the land cover class level (crop_ann, crop_per, manpast, rangeland, etc.)
#' and not directly per product. This function allocates the biodiversity impact
#' from cropland to individual crop products based on their area shares.
#' 
#' The indicator "bv" returns the BII-weighted area (higher = more biodiversity preserved),
#' allocated to each crop by its share of total cropland area.
#' 
#' The indicator "bii_loss" returns the biodiversity loss ((1-BII) * area) for aggregate
#' cropland/pasture, then allocates to individual products by area share. This represents
#' "share of biodiversity loss attributable to this crop based on its area share."
#' @examples
#' \dontrun{
#'   x <- embodiedBiodiversity(gdx, type = "all")
#'   # Bilateral flows
#'   xBilat <- embodiedBiodiversity(gdx, type = "flows", bilateral = TRUE)
#' }
#'

embodiedBiodiversity <- function(gdx,
                                 file = NULL,
                                 level = "reg",
                                 type = "all",
                                 indicator = "bv",
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
  
  if (!indicator %in% c("bv", "bii_loss")) {
    stop("indicator must be either 'bv' or 'bii_loss'")
  }
  
  # ==============================================================================
  # GET BIODIVERSITY VALUE DATA
  # ==============================================================================
  
  # Read biodiversity value per land cover class (in Mha, BII-weighted)
  ov_bv <- readGDX(gdx, "ov_bv", select = list(type = "level"), react = "silent")
  if (is.null(ov_bv)) {
    ov_bv <- readGDX(gdx, "ov44_bii", select = list(type = "level"), react = "silent")
  }
  if (is.null(ov_bv)) {
    stop("Biodiversity module data not found in gdx. This function requires a MAgPIE run with biodiversity module enabled.")
  }
  
  # Get crop biodiversity value (crop_ann + crop_per)
  cropBV <- dimSums(ov_bv[, , c("crop_ann", "crop_per")], dim = c(3.1, 3.2))
  
  # Get pasture biodiversity value (manpast + rangeland)
  pastBV <- dimSums(ov_bv[, , c("manpast", "rangeland")], dim = c(3.1, 3.2))
  
  # ==============================================================================
  # GET LAND AREA DATA FOR ALLOCATION
  # ==============================================================================
  
  # Get cropland area by product (for proportional allocation of crop BV)
  cropArea <- croparea(gdx, level = "cell", products = "kcr", product_aggr = FALSE,
                       water_aggr = TRUE)
  
  # Get total cropland area
  cropAreaTotal <- dimSums(cropArea, dim = 3)
  cropAreaTotal[cropAreaTotal == 0] <- 1  # avoid division by zero
  
  # Calculate crop area shares
  cropAreaShare <- cropArea / cropAreaTotal
  cropAreaShare[is.na(cropAreaShare)] <- 0
  
  # Get pasture area
  pastArea <- land(gdx, level = "cell")[, , "past"]
  
  # ==============================================================================
  # CALCULATE BV OR BII LOSS BY PRODUCT
  # ==============================================================================
  
  if (indicator == "bv") {
    # Allocate crop BV to individual crops by area share
    bvByCrop <- cropBV * cropAreaShare
    
    # For pasture: allocate to "pasture" product directly
    bvByPast <- setNames(pastBV, "pasture")
  } else {
    # indicator == "bii_loss"
    # Calculate BII loss at aggregate level first, then allocate by area share
    # BII loss = (1 - BII) * area = total_area - BV
    
    # Crop BII loss at cell level, then allocate by crop area share
    cropBIIloss <- cropAreaTotal - cropBV
    cropBIIloss[cropBIIloss < 0] <- 0  # ensure non-negative
    bvByCrop <- cropBIIloss * cropAreaShare
    
    # Pasture BII loss
    pastBIIloss <- pastArea - pastBV
    pastBIIloss[pastBIIloss < 0] <- 0
    bvByPast <- setNames(pastBIIloss, "pasture")
  }
  
  # Combine crops and pasture
  bvByProduct <- mbind(bvByCrop, bvByPast)
  
  # Aggregate from cell to regional level
  bvByProduct <- dimSums(bvByProduct, dim = 1.2)
  
  # ==============================================================================
  # CALCULATE BIODIVERSITY INTENSITY (BV per unit production)
  # ==============================================================================
  
  # Get production data
  prod <- production(gdx, level = "reg", product_aggr = FALSE, attributes = "dm")
  
  # Handle pasture name
  prodPast <- setNames(prod[, , "pasture"], "pasture")
  prod <- prod[, , "pasture", invert = TRUE]
  prod <- mbind(prod, prodPast)
  
  # Get common products
  commonProducts <- intersect(getItems(bvByProduct, dim = 3), getItems(prod, dim = 3))
  bvByProduct <- bvByProduct[, , commonProducts]
  prod <- prod[, , commonProducts]
  
  # Calculate intensity (BV per tDM)
  bvIntensity <- bvByProduct / prod
  bvIntensity[is.na(bvIntensity)] <- 0
  bvIntensity[is.infinite(bvIntensity)] <- 0
  
  # ==============================================================================
  # GET BILATERAL TRADE FLOWS
  # ==============================================================================
  
  # Use primary equivalents trade (converts livestock -> feed, secondary -> primary)
  trade <- tradedPrimariesBilateral(gdx, bilateral = TRUE, convFactor = "exporter",
                                    kastner = TRUE, level = "reg")
  trade <- dimSums(trade, dim = 3.1)
  
  # Filter to common products
  tradeProducts <- intersect(getItems(trade, dim = 3), commonProducts)
  trade <- trade[, , tradeProducts]
  bvIntensity <- bvIntensity[, , tradeProducts]
  bvByProduct <- bvByProduct[, , tradeProducts]
  prod <- prod[, , tradeProducts]
  
  # ==============================================================================
  # CALCULATE EMBODIED BIODIVERSITY IN TRADE
  # ==============================================================================
  
  # Rename importer dimension temporarily to allow multiplication by exporter only
  getItems(trade, dim = 1.2) <- paste0(getItems(trade, dim = 1.2), "_im")
  bvTraded <- trade * bvIntensity
  getItems(bvTraded, dim = 1.2) <- sub("_im$", "", getItems(bvTraded, dim = 1.2))
  
  # ==============================================================================
  # BILATERAL OUTPUT
  # ==============================================================================
  
  if (bilateral) {
    out <- bvTraded
    
    # Write to file if requested
    if (!is.null(file)) {
      write.magpie(out, file_name = file)
    }
    
    return(out)
  }
  
  # ==============================================================================
  # NON-BILATERAL: Calculate exports and imports
  # ==============================================================================
  
  # Production-based biodiversity footprint
  bvProd <- bvByProduct
  
  # Exports: sum over importing regions
  bvExport <- dimSums(bvTraded, dim = 1.2)
  
  # Imports: sum over exporting regions
  bvImport <- dimSums(bvTraded, dim = 1.1)
  
  # Consumption-based biodiversity footprint = production - exports + imports
  bvConsump <- bvProd - bvExport + bvImport
  
  # Prepare output based on requested type
  if (type == "production") {
    out <- add_dimension(bvProd, dim = 3.1, add = "accounting", nm = "production")
  } else if (type == "consumption") {
    out <- add_dimension(bvConsump, dim = 3.1, add = "accounting", nm = "consumption")
  } else if (type == "net-trade") {
    bvNetTrade <- bvConsump - bvProd
    out <- add_dimension(bvNetTrade, dim = 3.1, add = "accounting", nm = "net-trade")
  } else if (type == "all") {
    out <- mbind(
      add_dimension(bvProd, dim = 3.1, add = "accounting", nm = "production"),
      add_dimension(bvConsump, dim = 3.1, add = "accounting", nm = "consumption"),
      add_dimension(bvConsump - bvProd, dim = 3.1, add = "accounting", nm = "net-trade")
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
