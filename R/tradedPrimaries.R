#' @title tradedPrimaries
#' @description Converts trade flows into primary product equivalents by tracing
#'              secondary and livestock products back to their primary inputs.
#'              Trade flows are decomposed into three pathways:
#'              (1) direct primary trade, (2) primaries embodied in secondary products,
#'              and (3) primaries needed as feed for livestock.
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @details Processing shares and livestock feed baskets of the importing region are used to 
#'          calculate primary product requirements. This means that no footprint can be assigned 
#'          to secondaries or livestock products originating from the producer region's 
#'          processing pathways or feed baskets. With a bilateral trade implementation, these 
#'          differences between producer and importer could be tracked.
#'
#' @return MAgPIE object with primary product trade equivalents in dry matter (tDM)
#' @author Kristine Karstens, David M Chen
#' @seealso \code{\link{land}}, \code{\link{croparea}}, \code{\link{trade}}
#' 
#' @importFrom madrat toolConditionalReplace
#' @importFrom gdx2 readGDX
#' @examples
#' \dontrun{
#'   x <- tradedPrimaries(gdx)
#' }
#'
#' @export

tradedPrimaries <- function(gdx, file = NULL) {
    
  message("NOTE: Processing shares and livestock feed baskets of importing regions are used ",
          "to calculate primary product requirements. No footprint is assigned to secondaries ",
          "or livestock products from producer region processing pathways or feed baskets. ",
          "With bilateral trade implementation, these differences could be tracked.")
     
  # Get net trade flows (exports - imports) by region
  # TO-DO: Check again, if bilateral trade flows are available
  tradeFlows <- trade(gdx, type = "net-exports")
  #tradeFlows <- ???(gdx) # trade matrix with bilateral flows
    
  # ============================================================================
  # CALCULATE PRIMARY PRODUCT DEMANDS BY PRODUCT TYPE
  # ============================================================================

  # --------------------------------------------------------------------------
  # PRIMARY PRODUCTS: Direct trade of primary products
  # --------------------------------------------------------------------------
   
  # Primary trade already in primary products - use directly
  kcr                <- readGDX(gdx, "kcr")
  tradedKcrProducts  <- intersect(getNames(tradeFlows), kcr)
  primaryDemandsPrim <- tradeFlows[, , tradedKcrProducts]   
  
  # --------------------------------------------------------------------------
  # SECONDARY PRODUCTS: Trace back to primary product requirements
  # --------------------------------------------------------------------------
    
  # Get primary requirements per unit of secondary product (tDM primary / tDM secondary)
  # Using value-based allocation for co-product allocation
  primPerSecd <- primaryPerSecondary(gdx, level = "reg", allocation = "value")
    
  # Get secondary product trade flows
  ksd       <- readGDX(gdx, "ksd")
  secdTrade <- tradeFlows[, , ksd]
  simYears  <- getYears(secdTrade)
    
  # Calculate primary product demands from secondary trade
  # Result has dimensions: [region, year, secondary_product.primary_product]
  primaryDemandsSecd <- secdTrade * primPerSecd[, simYears, ]
  # Sum over secondary products to get total primary demand by primary product
  primaryDemandsSecd <- dimSums(primaryDemandsSecd, dim = 3.1)
    
  # --------------------------------------------------------------------------
  # LIVESTOCK PRODUCTS: Trace back to feed requirements
  # --------------------------------------------------------------------------
    
  # Get livestock product sets
  kli  <- readGDX(gdx, "kli")  # Livestock products (excluding fish)
  kap  <- readGDX(gdx, "kap")  # Animal products (including fish)
  kres <- readGDX(gdx, "kres")  
  kve  <- readGDX(gdx, "kve") # Land-use activities
  
  # Get feed baskets (tDM feed per tDM livestock product)
  # Dimensions: [region, year, livestock_product, feed_type]
  feedBaskets <- readGDX(gdx, "im_feed_baskets")
  getNames(feedBaskets, dim = "kap") <- paste0("kli_", getNames(feedBaskets, dim = "kap"))
  feedBaskets <- feedBaskets[, , paste0("kli_", kli)]
    
  # Get livestock product trade flows
  liTrade <- setNames(tradeFlows[, , kli], paste0("kli_", getNames(tradeFlows[, , kli]))) 
  simYears <- getYears(liTrade)    
  
  # Account for livestock-as-feed using iterative convergence
  # Solves: TotalLiDemand = liTrade + liInFeed(TotalLiDemand)
  # Equivalent to geometric series: Total = L + AL + A²L + A³L + ...
  # where A is the matrix of livestock-in-feed coefficients
  totalLiDemand <- liTrade
  for (iter in 1:10) {
    # Calculate all feed requirements for current livestock demand
    allFeedDemands <- feedBaskets[, simYears, ] * totalLiDemand
    # Extract only livestock products used as feed, summing across producing products
    liInFeed <- dimSums(allFeedDemands[, , kli], dim = 3.1)
    # Check convergence
    if (max(abs(liInFeed)) < 1e-6) break
    # Add indirect livestock demand
    totalLiDemand <- totalLiDemand + liInFeed
  }
  
  # Calculate final feed demands using total (direct + indirect) livestock demand
  feedDemands <- feedBaskets[, simYears, ] * totalLiDemand
  
  # Aggregate feed demands by type, converting to primary equivalents
  feedDemandsPrim <- dimSums(feedDemands[,, kve], dim = 3.1)
  # Remove livestock dimension from names, keeping only primary products
  getNames(feedDemandsPrim) <- gsub("\\..*$", "", getNames(feedDemandsPrim))
  # Sum across duplicate product names
  feedDemandsPrim <- dimSums(feedDemandsPrim, dim = 3, na.rm = TRUE)
  
  feedDemandsSecd <- dimSums(feedDemands[,, ksd] * primPerSecd[, simYears, ], dim = c(3.1, 3.2))
  feedDemandsKres <- feedDemands[,, kres]  # ignore residues for now
  
  # --------------------------------------------------------------------------
  # SUMMARY: Combine all primary demands into single output object
  # --------------------------------------------------------------------------
  # Create output structure with dimensions: [region, year, pathway.product]
  # where pathway ∈ {prim, secd, feed} and product ∈ kve (land-use activities)
  
  types          <- c("prim", "secd", "feed")
  dataDimNames   <- as.vector(outer(types, kve, paste, sep = "."))
  primaryDemands <- new.magpie(getRegions(primaryDemandsPrim),  
                               getYears(primaryDemandsPrim), 
                               names = dataDimNames, fill = 0)
  
  # Identify which primary products appear in each pathway
  kcrFromSecd <- intersect(getItems(primaryDemandsSecd, dim = 3), kve)
  kveFromFeed <- intersect(getNames(feedDemandsPrim), kve)
  kcrFromFeedSecd <- intersect(getItems(feedDemandsSecd, dim = 3), kve)
  
  # Populate primary pathway: direct trade of primary products
  primaryDemands[, , "prim"][, , tradedKcrProducts] <- primaryDemandsPrim
  
  # Populate secondary pathway: primaries from processing secondary products
  primaryDemands[, , "secd"][, , kcrFromSecd] <- primaryDemandsSecd[, , kcrFromSecd]
  
  # Populate feed pathway: primaries from livestock feed
  # Note: Feed can contain both direct primaries (kve) and primaries from 
  # secondary feed ingredients (ksd), so we add both components
  primaryDemands[, , "feed"][, , kveFromFeed] <- feedDemandsPrim[, , kveFromFeed]
  primaryDemands[, , "feed"][, , kcrFromFeedSecd] <- 
    primaryDemands[, , "feed"][, , kcrFromFeedSecd] + feedDemandsSecd[, , kcrFromFeedSecd]
                                 
  out(primaryDemands, file)
}
