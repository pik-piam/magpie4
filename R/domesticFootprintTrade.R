#' @title domesticFootprintTrade
#' @description Calculates embodied land in trade using the domestic technology assumption.
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg", "glo", "regglo"
#' @param type "exports", "imports", or "net-trade" (default)
#' @param productsAggr Aggregation of primary products; default is TRUE
#' @param maxIntensityRatio Maximum allowed ratio of regional to historical (1995) land intensity.
#'                          Regional land intensities exceeding this factor times the 1995 value
#'                          are capped. This same value is used for filling NAs. Use NULL for no capping.
#'                          Default is 5 (caps at 5× 1995 values).
#'
#' @return MAgPIE object with embodied land in trade (Mha)
#'
#' @details The domestic technology assumption values all traded goods using each region's
#'          own processing shares, feed baskets, and land intensity (yields). For exports
#'          this represents the actual land footprint. For imports this is a counterfactual:
#'          how much land would the region need if it produced its imports domestically?
#'
#'          Trade flows are decomposed into primary product equivalents via three pathways:
#'          (1) direct primary trade, (2) primaries embodied in secondary products,
#'          (3) primaries needed as livestock feed. Land intensity (ha/tDM) is then
#'          applied per primary product.
#'          
#'          Regional land intensities are filled and capped using historical (1995) FAO 
#'          yields as reference. This ensures comparability across different model runs 
#'          while accounting for regional differences in agricultural potential.
#'
#'          This is a consistent approach with net trade data (no bilateral flows).
#'          With bilateral trade data, different basis for land intensity could be applied.
#'
#' @author Kristine Karstens
#' @seealso \code{\link{tradedPrimaries}}
#'
#' @importFrom magclass mbind dimSums setNames getItems getYears getRegions new.magpie
#' @importFrom magpie4 production land croparea
#' @importFrom madrat toolConditionalReplace
#' @importFrom gdx readGDX
#' @examples
#' \dontrun{
#'   x <- domesticFootprintTrade(gdx, type = "net-trade")
#' }
#'
#' @export

domesticFootprintTrade <- function(gdx,
                                   file = NULL,
                                   level = "reg",
                                   type = "net-trade",
                                   productsAggr = TRUE,
                                   maxIntensityRatio = 5) {

  if (!type %in% c("exports", "imports", "net-trade")) {
    stop("type must be one of: 'exports', 'imports', 'net-trade'")
  }

  # ============================================================================
  # STEP 1: Get primary product trade equivalents from all pathways
  # ============================================================================

  tradedPrim <- tradedPrimaries(gdx)

  # Separate into exports (positive) and imports (negative → made positive)
  exports <- tradedPrim
  exports[exports < 0] <- 0
  
  imports <- tradedPrim
  imports[imports > 0] <- 0
  imports <- abs(imports)

  # ============================================================================
  # STEP 2: Calculate land intensity (ha/tDM) per primary product
  # ============================================================================

  prod     <- production(gdx, level = "reg", product_aggr = FALSE, attributes = "dm")
  cropLand <- croparea(gdx, level = "reg", products = "kcr",
                       product_aggr = FALSE, water_aggr = TRUE)
  pastLand <- land(gdx, level = "reg", types = "past", subcategories = FALSE)
  land     <- mbind(cropLand, setNames(pastLand, "pasture"))
  landIntensity <- toolConditionalReplace(land / prod[, , getItems(land, dim = 3)],
                                          c("is.na()", "== 0", "is.infinite()"), NA)
  
  histCropIntensity <- 1 / readGDX(gdx, "f14_fao_yields_hist")[, "y1995", ]
  histPastIntensity <- 1 / readGDX(gdx, "f14_pyld_hist")[, "y1995", ]
  histLandIntensity <- mbind(histCropIntensity, setNames(histPastIntensity, "pasture"))
  histLandIntensity <- magpie_expand(collapseDim(histLandIntensity), landIntensity)

  # Apply consistent capping/filling using historical reference
  if (!is.null(maxIntensityRatio)) {
    cappedHistIntensity <- histLandIntensity * maxIntensityRatio
    landIntensity[is.na(landIntensity)] <- cappedHistIntensity[is.na(landIntensity)]
    landIntensity <- pmin(landIntensity, cappedHistIntensity)
  } else {
    # No capping: use historical values only for NA filling
    landIntensity[is.na(landIntensity)] <- histLandIntensity[is.na(landIntensity)]
  }

  # ============================================================================
  # STEP 3: Calculate embodied land
  # ============================================================================

  footprintCrops <- intersect(getNames(tradedPrim, dim = 2), getNames(landIntensity))

  exports <- exports[, , footprintCrops]
  imports <- imports[, , footprintCrops]
  landIntensity <- landIntensity[, , footprintCrops]

  # Calculate embodied land in trade
  landExports <- exports * landIntensity
  landImports <- imports * landIntensity

  # ============================================================================
  # STEP 4: Select output type
  # ============================================================================

  if (type == "exports") {
    out <- landExports
  } else if (type == "imports") {
    out <- landImports
  } else {
    # net-trade: positive = net land exporter, negative = net land importer
    out <- landExports - landImports
  }

  # Aggregate products if requested
  if (productsAggr == TRUE) {
    out <- dimSums(out, dim = 3)
  } else if (productsAggr == "flowtype") {
    out <- dimSums(out, dim = "flowtype")
  } else if (productsAggr == "product") {
    out <- dimSums(out, dim = "product")
  }

  # Aggregate to requested level
  if (level != "reg") {
    out <- gdxAggregate(gdx = gdx, x = out, weight = NULL, to = level, absolute = TRUE)
  }

  out(out, file)
}
