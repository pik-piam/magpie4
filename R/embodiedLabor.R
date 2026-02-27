#' @title embodiedLabor
#' @description Calculates production-based and consumption-based (embodied) labor footprint 
#' accounting using bilateral trade flows. Labor costs or labor hours are allocated to traded 
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
#' @param indicator Which indicator to report: "costs" (labor costs in million US$17) or
#'   "hours" (labor hours in million hours per year). Default is "costs".
#' @param bilateral Logical; if TRUE, returns bilateral flows with dimensions 
#'   (exporter.importer, year, product) instead of regional totals (default FALSE)
#'
#' @return Embodied labor as MAgPIE object.
#'   When indicator="costs": million US$17 (labor costs)
#'   When indicator="hours": million hours per year (labor hours)
#'   When bilateral=FALSE: dimensions are (region, year, accounting.product).
#'   When bilateral=TRUE: dimensions are (exporter.importer, year, product).
#' @author David M Chen
#' @seealso \code{\link{laborCostsEndo}}, \code{\link{factorCosts}}, \code{\link{trade}},
#'   \code{\link{hourlyLaborCosts}}, \code{\link{totalHoursWorked}}
#' @importFrom magclass collapseNames mbind dimSums dimOrder setNames getItems getYears add_dimension collapseDim
#' @importFrom magpie4 production laborCostsEndo factorCosts hourlyLaborCosts
#' @examples
#' \dontrun{
#'   x <- embodiedLabor(gdx, type = "all")
#'   # Bilateral flows in labor hours
#'   xBilat <- embodiedLabor(gdx, type = "flows", indicator = "hours", bilateral = TRUE)
#' }
#'

embodiedLabor <- function(gdx,
                         file = NULL,
                         level = "reg",
                         type = "all",
                         indicator = "costs",
                         bilateral = FALSE) {
  
  # ==============================================================================
  # VALIDATE PARAMETERS
  # ==============================================================================
  
  if (!indicator %in% c("costs", "hours")) {
    stop("indicator must be either 'costs' or 'hours'")
  }
  
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
  # GET PRODUCTION AND LABOR COST DATA
  # ==============================================================================
  
  # Get production data
  prod <- production(gdx, level = "reg", products = "kcr", 
       product_aggr = FALSE, attributes = "dm")
  prodPast <- production(gdx, level = "reg", products = "pasture", 
       product_aggr = FALSE, attributes = "dm")
  prodLi <- production(gdx, level = "reg", products = "kli", 
       product_aggr = FALSE, attributes = "dm")

  # Handle pasture name to match land function
  prodPast <- setNames(prodPast, "past")
  prod <- mbind(prod, prodPast, prodLi)
  
  # Get labor costs for crops (per product)
  laborCropsCosts <- tryCatch({
    laborCostsEndo(gdx, products = "kcr", level = "iso")
  }, error = function(e) {
    # Fallback: use factorCosts if laborCostsEndo doesn't work
    fc <- factorCosts(gdx, products = "kcr", level = "reg")  ## this one is not per crop
    fc[, , "labor_costs"]
  })
  laborCropsCosts <- collapseNames(laborCropsCosts)
  
  # Get labor costs for livestock (per product)
  laborLivestockCosts <- tryCatch({
    laborCostsEndo(gdx, products = "kli", level = "reg")
  }, error = function(e) {
    # Fallback: use factorCosts if laborCostsEndo doesn't work
    fc <- factorCosts(gdx, products = "kli", level = "reg")
    fc[, , "labor_costs"]
  })
  laborLivestockCosts <- collapseNames(laborLivestockCosts)

    # Get labor costs for pasture
  laborPasture <- tryCatch({
    laborCostsEndo(gdx, products = "pasture", level = "reg")
  }, error = function(e) {
    # Fallback: use factorCosts if laborCostsEndo doesn't work
    fc <- factorCosts(gdx, products = "pasture", level = "reg")
    fc[, , "labor_costs"]
  })
     laborPasture <- setNames(laborPasture[, , "Pasture"], "past")


  
  # Combine crop and livestock labor costs
  laborCosts <- mbind(laborCropsCosts, laborLivestockCosts)
  
  # ==============================================================================
  # CONVERT TO LABOR HOURS IF REQUESTED
  # ==============================================================================
  
  if (indicator == "hours") {
    # Get hourly labor costs (USD17MER per hour)
    hourlyCosts <- hourlyLaborCosts(gdx, level = "reg")
    
    if (is.null(hourlyCosts)) {
      stop("Hourly labor costs not available in gdx. ",
           "This function requires a MAgPIE run with employment module enabled.")
    }
    
    # Convert labor costs (million USD) to labor hours (million hours)
    # labor_hours = labor_costs / hourly_costs
    # Note: laborCosts is in million USD, hourlyCosts is in USD/hour
    # So result is in million hours
    laborCosts <- laborCosts / hourlyCosts
    laborCosts[is.na(laborCosts)] <- 0
    laborCosts[is.infinite(laborCosts)] <- 0
  }
  
  # Get common products between production and labor costs
  commonProducts <- intersect(getItems(prod, dim = 3), getItems(laborCosts, dim = 3))
  prod <- prod[, , commonProducts]
  laborCosts <- laborCosts[, , commonProducts]
  
  # Calculate labor intensity (labor cost or hours per unit production)
  laborIntensity <- laborCosts / prod
  laborIntensity[is.na(laborIntensity)] <- 0
  laborIntensity[is.infinite(laborIntensity)] <- 0
  
  # ==============================================================================
  # GET BILATERAL TRADE FLOWS
  # ==============================================================================
  
  # For crops: use primary equivalents trade
  tradePrimary <- tradedPrimariesBilateral(gdx, bilateral = TRUE, convFactor = "exporter",
                                           kastner = TRUE, level = "reg")
  tradePrimary <- dimSums(tradePrimary, dim = 3.1)
  
  # For livestock: use direct trade (Kastner-adjusted)
  tradeRaw <- tryCatch({
    collapseNames(readGDXBilateral(gdx = gdx, "ov21_trade"))
  }, error = function(e) {
    stop("Bilateral trade data not found in gdx. This function requires a MAgPIE run with bilateral trade module.")
  })
  tradeRaw <- tradeRaw[, , "level", drop = TRUE]
  tradeLivestock <- tradeKastner(gdx = gdx, trade = tradeRaw, level = "reg",
                                 products = "kall", attributes = "dm")
  
  # Identify livestock vs crop products
  kli <- readGDX(gdx, "kli")
  livProducts <- intersect(kli, commonProducts)
  cropProducts <- setdiff(commonProducts, kli)
  
  # Filter to products in trade
  cropProductsInTrade <- intersect(cropProducts, getItems(tradePrimary, dim = 3))
  livProductsInTrade <- intersect(livProducts, getItems(tradeLivestock, dim = 3))
  
  # ==============================================================================
  # HELPER FUNCTION: Calculate embodied labor for a trade matrix
  # ==============================================================================
  
  .calcEmbodiedLabor <- function(tradeMatrix, products, intensity, bilateral = FALSE) {
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
    laborTraded <- tradeMatrix * intensity
    getItems(laborTraded, dim = 1.2) <- sub("_im$", "", getItems(laborTraded, dim = 1.2))
    
    if (bilateral) {
      return(list(bilateral = laborTraded))
    } else {
      # Exports: sum over importing regions
      laborExp <- dimSums(laborTraded, dim = 1.2)
      # Imports: sum over exporting regions
      laborImp <- dimSums(laborTraded, dim = 1.1)
      
      return(list(export = laborExp, import = laborImp))
    }
  }
  
  # ==============================================================================
  # CALCULATE EMBODIED LABOR
  # ==============================================================================
  
  # Calculate for crops (using primary equivalents trade)
  cropResult <- .calcEmbodiedLabor(tradePrimary, cropProductsInTrade, laborIntensity, bilateral)
  
  # Calculate for livestock (using direct trade)
  livResult <- .calcEmbodiedLabor(tradeLivestock, livProductsInTrade, laborIntensity, bilateral)
  
  # ==============================================================================
  # COMBINE RESULTS
  # ==============================================================================
  
  if (bilateral) {
    # Combine bilateral flows
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
  # NON-BILATERAL: Combine results into regional totals
  # ==============================================================================
  
  # Production-based labor costs
  laborProd <- laborCosts[, , c(cropProductsInTrade, livProductsInTrade)]
  
  laborExport <- laborProd * 0
  laborImport <- laborProd * 0
  
  if (!is.null(cropResult$export)) {
    laborExport[, , cropProductsInTrade] <- cropResult$export
    laborImport[, , cropProductsInTrade] <- cropResult$import
  }
  if (!is.null(livResult$export)) {
    laborExport[, , livProductsInTrade] <- livResult$export
    laborImport[, , livProductsInTrade] <- livResult$import
  }
  
  # Consumption-based labor footprint = production - exports + imports
  laborConsump <- laborProd - laborExport + laborImport
  
  # Prepare output based on requested type
  if (type == "production") {
    out <- add_dimension(laborProd, dim = 3.1, add = "accounting", nm = "production")
  } else if (type == "consumption") {
    out <- add_dimension(laborConsump, dim = 3.1, add = "accounting", nm = "consumption")
  } else if (type == "net-trade") {
    laborNetTrade <- laborConsump - laborProd
    out <- add_dimension(laborNetTrade, dim = 3.1, add = "accounting", nm = "net-trade")
  } else if (type == "all") {
    out <- mbind(
      add_dimension(laborProd, dim = 3.1, add = "accounting", nm = "production"),
      add_dimension(laborConsump, dim = 3.1, add = "accounting", nm = "consumption"),
      add_dimension(laborConsump - laborProd, dim = 3.1, add = "accounting", nm = "net-trade")
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
