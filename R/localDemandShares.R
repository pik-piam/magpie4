#' @title localDemandShares
#' @description returns labor and capital cost share out of factor costs (i.e. labor + capital)
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation to report employment ("reg", "glo" or "regglo")
#' @type "prod" or "dem". the former indicates the share of production consumed in its own cluster
#' while the latter indicates how much cluster-level demand is satisfied by local consumption
#' @param file a file name the output should be written to using write.magpie
#' @param type "prod" or "dem". the former indicates the share of production consumed in its own cluster
#' while the latter indicates how much cluster-level demand is satisfied by local consumption
#' @param product_aggr sum over products if TRUE
#' @param urb_aggr sum over products if TRUE
#' @param fvc_aggr sum over products if TRUE
#' @return share or level of food consumed locally, in disaggregated transport module
#' @author David M Chen
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- localDemandShares(gdx)
#' }

localDemandShares <- function(gdx, type = "prod", level = "reg", product_aggr = TRUE, 
                                   urb_aggr = TRUE, fvc_aggr = TRUE, file = NULL) {

   localFoodConsumed <- readGDX(gdx, "ov40_dem_for_local",
                               select = list(type = "level"), react = "silent")
#localFoodConsumed[,,"foddr"]  %>% sum()

  if (type == "dem") {
  clusterImports <- readGDX(gdx, "ov40_cell_import",
                               select = list(type = "level"), react = "silent")
    
  #total cluster demand is the sum of amount demanded from local production + imports^
  localDemand <- localFoodConsumed + clusterImports
   if (product_aggr) { 
    localDemand <- dimSums(localDemand, dim = 3.1)
    localFoodConsumed <- dimSums(localFoodConsumed, dim = 3.1)
           }
    if (urb_aggr) { 
    localDemand <- dimSums(localDemand, dim = "urb")
    localFoodConsumed <- dimSums(localFoodConsumed, dim = "urb")
           }
   if (fvc_aggr) { 
    localDemand <- dimSums(localDemand, dim = "fvc")
    localFoodConsumed <- dimSums(localFoodConsumed, dim = "fvc")
           }
  #localDemand[,,"foddr"]  %>% sum()
  shr <- localFoodConsumed / localDemand 
  weight <- localDemand
  shr[is.na(shr)] <- 1
  
   } else if (type == "prod"){

   pr <- production(gdx, products = "kcr", level = "cell")   
   prli <-  production(gdx, products = "kli", level = "cell")   

   pr <- mbind(pr, prli)

   localFoodConsumed <- localFoodConsumed[, , getItems(pr, dim = 3)]
        
        #amount locally consumed
    shr <- round(dimSums(localFoodConsumed, 
                 dim = c(3.2,3.3)), 4) / round(pr, 4)
   
   shr[is.na(shr)] <- 1

     if(product_aggr) { 
    rel <- data.frame("cr" = getNames(shr), "tot" = "k")
    shr <- toolAggregate(shr, rel = rel, weight = pr, from  ="cr", to = "tot", wdim = 3.1, dim = 3)
    pr <- dimSums(pr, dim = 3)

     }            
  weight <- pr 

         }
  

  if (level!="cell"){
   shr <- superAggregate(shr, aggr_type="weighted_mean",
                             weight = weight + 1e-6,
                             level = level)
  }

  out(shr, file)
}
