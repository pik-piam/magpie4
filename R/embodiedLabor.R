#' @title embodiedLabor
#' @description Calculates production-based and consumption-based (embodied) labor footprint 
#' accounting using bilateral trade flows. Employment (number of people) is allocated to traded 
#' products based on production ratios and bilateral trade patterns.
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
#' @param bilateral Logical; if TRUE, returns bilateral flows with dimensions 
#'   (exporter.importer, year, product) instead of regional totals (default FALSE)
#'
#' @return Embodied employment as MAgPIE object (number of people).
#'   When bilateral=FALSE: dimensions are (region, year, accounting.product).
#'   When bilateral=TRUE: dimensions are (exporter.importer, year, product).
#' @author David M Chen
#' @seealso \code{\link{agEmployment}}, \code{\link{trade}},
#'   \code{\link{embodiedLand}}
#' @importFrom magclass collapseNames mbind dimSums dimOrder setNames getItems getYears add_dimension collapseDim
#' @importFrom magpie4 production agEmployment factorCosts
#' @examples
#' \dontrun{
#'   x <- embodiedLabor(gdx, type = "all")
#'   # Bilateral flows
#'   xBilat <- embodiedLabor(gdx, type = "flows", bilateral = TRUE)
#' }
#'

embodiedLabor <- function(gdx,
                         file = NULL,
                         level = "reg",
                         type = "all",
                         bilateral = FALSE) {
  
  # ==============================================================================
  # VALIDATE PARAMETERS
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
  # GET PRODUCTION DATA
  # ==============================================================================
  
  prod <- production(gdx, level = "reg", products = "kcr", 
                     product_aggr = FALSE, attributes = "dm")
  prodPast <- production(gdx, level = "reg", products = "pasture", 
                         product_aggr = FALSE, attributes = "dm")
  prodLi <- production(gdx, level = "reg", products = "kli", 
                       product_aggr = FALSE, attributes = "dm")
  
  # Keep pasture as "pasture" to match kve set / trade naming
  prod <- mbind(prod, prodPast, prodLi)
  
  # ==============================================================================
  # GET EMPLOYMENT DATA (number of people employed, by product)
  # ==============================================================================
  
  # Get per-product employment for kcr + kli from agEmployment
  employment <- agEmployment(gdx, type = "absolute", prodAggr = FALSE, level = "reg")
  
  # Add pasture employment: use factorCosts labor shares to split 
  # a portion of total employment to pasture
  totalEmpl <- readGDX(gdx, "ov36_employment", select = list(type = "level"), react = "silent")
  
  laborCostsPast <- factorCosts(gdx, products = "pasture", level = "reg")[, , "labor_costs", drop = TRUE]
  laborCostsCrops <- factorCosts(gdx, products = "kcr", level = "reg")[, , "labor_costs", drop = TRUE]
  laborCostsLi <- factorCosts(gdx, products = "kli", level = "reg")[, , "labor_costs", drop = TRUE]
  
  pastShare <- laborCostsPast / (laborCostsPast + laborCostsCrops + laborCostsLi)
  pastShare[is.na(pastShare)] <- 0
  pastShare[is.infinite(pastShare)] <- 0
  
  emplPast <- setNames(totalEmpl * pastShare, "pasture")
  employment <- mbind(employment, emplPast)
  
  # ==============================================================================
  # CALCULATE EMPLOYMENT INTENSITY (people per unit production)
  # ==============================================================================
  
  commonProducts <- intersect(getItems(prod, dim = 3), getItems(employment, dim = 3))
  prod <- prod[, , commonProducts]
  employment <- employment[, , commonProducts]
  
  # Employment intensity: people per Mt DM
  emplIntensity <- employment / prod
  emplIntensity[is.na(emplIntensity)] <- 0
  emplIntensity[is.infinite(emplIntensity)] <- 0
  
  # ==============================================================================
  # GET BILATERAL TRADE FLOWS
  # ==============================================================================
  
  # For crops: use primary equivalents trade (includes pasture via feed pathway)
  # Keep pathway disaggregation (prim/secd/feed)
  tradePrimary <- tradedPrimariesBilateral(gdx, bilateral = TRUE, convFactor = "exporter",
                                           kastner = TRUE, level = "reg")
  # Do NOT collapse pathway dimension
  
  # For livestock: use direct trade (Kastner-adjusted)
  tradeRaw <- tryCatch({
    collapseNames(readGDXBilateral(gdx = gdx, "ov21_trade"))
  }, error = function(e) {
    stop("Bilateral trade data not found in gdx. This function requires a MAgPIE run with bilateral trade module.")
  })
  tradeRaw <- tradeRaw[, , "level", drop = TRUE]
  tradeLivestock <- tradeKastner(gdx = gdx, trade = tradeRaw, level = "reg",
                                 products = "kall", attributes = "dm")
  # Add "direct" pathway dimension to livestock trade
  tradeLivestock <- add_dimension(tradeLivestock, dim = 3.1, add = "pathway", nm = "direct")
  
  # Identify livestock vs crop products
  kli <- readGDX(gdx, "kli")
  livProducts <- intersect(kli, commonProducts)
  cropProducts <- setdiff(commonProducts, kli)
  
  # Filter to products in trade (using product subdimension)
  cropProductsInTrade <- intersect(cropProducts, unique(getItems(tradePrimary, dim = 3.2)))
  livProductsInTrade <- intersect(livProducts, unique(getItems(tradeLivestock, dim = 3.2)))
  
  # ==============================================================================
  # CALCULATE EMBODIED EMPLOYMENT IN TRADE FLOWS
  # Trade has dim 3 = pathway.product; emplIntensity has dim 3 = product
  # magclass broadcasts intensity across pathway subdimension
  # ==============================================================================
  
  # --- Crops (including pasture): use primary equivalents trade ---
  if (length(cropProductsInTrade) > 0) {
    tradeCrops <- tradePrimary[, , cropProductsInTrade]
    getItems(tradeCrops, dim = 1.2) <- paste0(getItems(tradeCrops, dim = 1.2), "_im")
    emplTradedCrops <- tradeCrops * emplIntensity[, , cropProductsInTrade]
    getItems(emplTradedCrops, dim = 1.2) <- sub("_im$", "", getItems(emplTradedCrops, dim = 1.2))
  } else {
    emplTradedCrops <- NULL
  }
  
  # --- Livestock: use direct trade ---
  if (length(livProductsInTrade) > 0) {
    tradeLiv <- tradeLivestock[, , livProductsInTrade]
    getItems(tradeLiv, dim = 1.2) <- paste0(getItems(tradeLiv, dim = 1.2), "_im")
    emplTradedLiv <- tradeLiv * emplIntensity[, , livProductsInTrade]
    getItems(emplTradedLiv, dim = 1.2) <- sub("_im$", "", getItems(emplTradedLiv, dim = 1.2))
  } else {
    emplTradedLiv <- NULL
  }
  
  # ==============================================================================
  # BILATERAL OUTPUT (with pathway.product dims)
  # ==============================================================================
  
  if (bilateral) {
    out <- NULL
    if (!is.null(emplTradedCrops)) out <- emplTradedCrops
    if (!is.null(emplTradedLiv)) {
      out <- if (is.null(out)) emplTradedLiv else mbind(out, emplTradedLiv)
    }
    
    if (!is.null(file)) write.magpie(out, file_name = file)
    return(out)
  }
  
  # ==============================================================================
  # NON-BILATERAL: Calculate production, consumption, net-trade
  # Output retains pathway dimension (production/direct/prim/secd/feed)
  # ==============================================================================
  
  # Production-based employment — no pathway dimension (pathway only applies to trade)
  emplProd <- employment[, , c(cropProductsInTrade, livProductsInTrade)]
  
  # Build export/import from trade results (already have pathway dims)
  emplExportComponents <- list()
  emplImportComponents <- list()
  
  if (!is.null(emplTradedCrops)) {
    emplExportComponents[["crop"]] <- dimSums(emplTradedCrops, dim = 1.2)
    emplImportComponents[["crop"]] <- dimSums(emplTradedCrops, dim = 1.1)
  }
  if (!is.null(emplTradedLiv)) {
    emplExportComponents[["liv"]] <- dimSums(emplTradedLiv, dim = 1.2)
    emplImportComponents[["liv"]] <- dimSums(emplTradedLiv, dim = 1.1)
  }
  
  emplExport <- if (length(emplExportComponents) > 0) mbind(emplExportComponents) else NULL
  emplImport <- if (length(emplImportComponents) > 0) mbind(emplImportComponents) else NULL
  
  # Net trade = imports - exports (keeps pathway: direct/prim/secd/feed)
  if (!is.null(emplExport) && !is.null(emplImport)) {
    emplNetTrade <- emplImport - emplExport
    emplConsump <- emplProd + dimSums(emplNetTrade, dim = 3.1)
  } else {
    emplNetTrade <- emplProd * 0
    emplConsump <- emplProd
  }
  
  # Prepare output based on requested type
  # Production/consumption: dim 3 = accounting.product
  # Net-trade: dim 3 = accounting.pathway.product (preserves prim/secd/feed/direct)
  # All: dim 3 = accounting.product (pathway collapsed for consistency)
  if (type == "production") {
    out <- add_dimension(emplProd, dim = 3.1, add = "accounting", nm = "production")
  } else if (type == "consumption") {
    out <- add_dimension(emplConsump, dim = 3.1, add = "accounting", nm = "consumption")
  } else if (type == "net-trade") {
    out <- add_dimension(emplNetTrade, dim = 3.1, add = "accounting", nm = "net-trade")
  } else if (type == "all") {
    out <- mbind(
      add_dimension(emplProd, dim = 3.1, add = "accounting", nm = "production"),
      add_dimension(emplConsump, dim = 3.1, add = "accounting", nm = "consumption"),
      add_dimension(dimSums(emplNetTrade, dim = 3.1), dim = 3.1, add = "accounting", nm = "net-trade")
    )
  } else {
    stop("Invalid type. Choose from: 'production', 'consumption', 'net-trade', or 'all'")
  }
  
  # Apply regional aggregation if requested
  if (level != "reg") {
    out <- superAggregate(out, aggr_type = "sum", level = level, na.rm = TRUE)
  }
  
  if (!is.null(file)) write.magpie(out, file_name = file)
  
  return(out)
}
