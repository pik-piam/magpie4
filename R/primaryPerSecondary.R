#' @title primaryPerSecondary
#' @description Calculates the amount of primary products needed per unit of secondary
#'              (and tertiary) product, accounting for processing chains and co-product allocation.
#'              This function traces back from secondary/tertiary products through processing
#'              stages to the original primary crop inputs.
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#'              "regglo" (regional and global) or any other aggregation level defined in gdxAggregate
#' @param allocation Method for allocating primary product use when co-products are produced.
#'                   Options:
#'                   "mass" (default): Allocation based on dry matter content of outputs
#'                   "value": Allocation based on economic value of outputs
#'                   "none": No allocation - full primary input attributed to each output
#'                             (sum of allocations > 100% when co-products exist)
#' 
#' @return MAgPIE object containing the amount of primary product needed per unit of secondary product (tDM/tDM)
#'
#' @author Kristine Karstens, David M Chen
#' @seealso \code{\link{production}}, \code{\link{trade}}
#' @importFrom magclass collapseNames mbind dimSums setNames getItems getYears add_dimension getNames
#' @importFrom gdx2 readGDX
#' @examples
#' \dontrun{
#' x <- primaryPerSecondary(gdx)
#' }
#' @export

primaryPerSecondary <- function(gdx,
                                file = NULL,
                                level = "reg",
                                allocation = "mass"){

  # Validate allocation method
  if (!allocation %in% c("mass", "value", "none")) {
    stop("allocation must be one of: 'mass', 'value', or 'none'")
  }

  # Read required sets from GDX
  ksd       <- readGDX(gdx, "ksd")           # Secondary products
  kcr       <- readGDX(gdx, "kcr")           # Crop products
  kpr       <- readGDX(gdx, "kpr")           # Products that can be processed
  primCrops <- intersect(kcr, kpr)           # Primary crops that can be processed
  simYears  <- readGDX(gdx, "t")             # Simulation years

  # Read processing parameters
  procShares <- readGDX(gdx, "f20_processing_shares")[, simYears, ] # Which primary produces which secondary
  procConvF  <- readGDX(gdx, "f20_processing_conversion_factors")[, simYears, ] # Conversion factors

  # Rename kpr dimension to distinguish from ksd
  getNames(procShares, dim = "kpr") <- paste0("kpr_", getNames(procShares, dim = "kpr"))
  getNames(procConvF, dim = "kpr")  <- paste0("kpr_", getNames(procConvF, dim = "kpr"))

  # ================================================================================
  # STEP 1: Calculate allocation weights based on chosen method
  # ================================================================================

  if (allocation == "mass") {
    # Mass-based allocation: Use conversion factors (DM output per DM input)
    # Allocation weight = output_mass / sum(all_output_masses)
    allocWeights <- procConvF / dimSums(procConvF, dim = "ksd")
    # Replace NaN with 0 (occurs when no outputs)
    allocWeights[is.nan(allocWeights)] <- 0
    allocWeights[is.infinite(allocWeights)] <- 0

  } else if (allocation == "value") {
    # Value-based allocation: Use economic value of outputs
    # Allocation weight = (output_mass * price) / sum(output_mass * price for all outputs)
    prices <- readGDX(gdx, "f15_prices_initial")[, , ksd]

    # Value = mass * price for each product
    productValues <- procConvF * prices
    
    # Normalize to get allocation weights
    allocWeights <- productValues / dimSums(productValues, dim = "ksd")
    allocWeights[is.nan(allocWeights)] <- 0
    allocWeights[is.infinite(allocWeights)] <- 0

  } else if (allocation == "none") {
    # No allocation: Each output gets full attribution of all inputs
    # Create dummy with ones where conversion factors are non-zero
    allocWeights <- procConvF
    allocWeights[allocWeights != 0] <- 1
  }

  # ================================================================================
  # STEP 2: Calculate primary per secondary (direct processing)
  # ================================================================================
  # For each secondary product, how much of each primary product is needed?
  # Formula: (1 / conversion_factor) * processing_share * allocation_weight

  # Inverse of conversion factor = primary input per unit secondary output
  primPerSecd <- procShares * allocWeights / procConvF

  # Handle division by zero
  primPerSecd[is.nan(primPerSecd)] <- 0
  primPerSecd[is.infinite(primPerSecd)] <- 0

  # Sum over processing types to get total primary per secondary
  primPerSecd <- dimSums(primPerSecd, dim = "processing20")

  # ================================================================================
  # STEP 3: Handle tertiary processing (secondary products that are further processed)
  # ================================================================================
  # Some secondary products (ksd) can also be processed further (are in kpr)
  # e.g., sugar, molasses, oils can be inputs to further processing

  secdPrims <- intersect(kpr, ksd)  # Secondary products that can be processed further
  tertTest  <- dimSums(procConvF[,,paste0("kpr_", secdPrims)], dim = c("processing20", "kpr"))
  tertSecds <- where(tertTest > 0)$true$data

  if (length(secdPrims) > 0) {
    # Extract relationships where secondary products are inputs to tertiary processing
    secdPerTert <- primPerSecd[, , paste0("kpr_", secdPrims)][, , tertSecds]

    # Rename dimensions to track the processing chain
    getNames(secdPerTert, dim = "ksd") <- paste0("ktr_", getNames(secdPerTert, dim = "ksd"))
    getNames(secdPerTert, dim = "kpr") <- gsub("kpr_", "", getNames(secdPerTert, dim = "kpr"))
    getSets(secdPerTert, fulldim = FALSE)[3] <- "ktr.ksd"

    # Calculate primary needed for tertiary via secondary
    # tertiary output needs secondary input, which needs primary input
    primPerTert <- secdPerTert * primPerSecd[, simYears, secdPrims]

    # Handle division by zero
    primPerTert[is.nan(primPerTert)] <- 0
    primPerTert[is.infinite(primPerTert)] <- 0

    # Sum over the intermediate secondary processing step
    primPerTert <- dimSums(primPerTert, dim = "ksd")

    # Clean up dimension names
    getNames(primPerTert, dim = "kpr") <- gsub("kpr_", "", getNames(primPerTert, dim = "kpr"))
    getNames(primPerTert, dim = "ktr") <- gsub("ktr_", "", getNames(primPerTert, dim = "ktr"))

    # Combine direct secondary and tertiary calculations
    # For products that are both secondary and tertiary, we need the total
    getNames(primPerSecd, dim = "kpr") <- gsub("kpr_", "", getNames(primPerSecd, dim = "kpr"))

    # Add tertiary contributions to the direct secondary and reduce to real primary products
    primPerSecd[, , tertSecds] <- primPerSecd[, , tertSecds] + primPerTert[, , tertSecds]
    realPrim    <- setdiff(kpr, ksd)
    primPerSecd <- primPerSecd[, , realPrim]

  } else {
    # No tertiary processing, just clean up names and reduce to real primary products
    getNames(primPerSecd, dim = "kpr") <- gsub("kpr_", "", getNames(primPerSecd, dim = "kpr"))
    realPrim    <- setdiff(kpr, ksd)
    primPerSecd <- primPerSecd[, , realPrim]
  }

  # ================================================================================
  # STEP 4: Filter to only primary crops
  # ================================================================================
  x <- primPerSecd[, , primCrops]

  # ================================================================================
  # STEP 5: Apply regional aggregation if requested
  # ================================================================================
  if (level != "reg") {
    x <- gdxAggregate(x, aggr_type = level, na.rm = TRUE)
  }

  out(x, file)
}
