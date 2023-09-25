#' @title localDemandShares
#' @description reports local demand and production shares based on local consumption
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation to report employment ("reg", "glo" or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @param type "prod" or "dem". the former indicates the share of production consumed in its own cluster
#' while the latter indicates how much cluster-level demand is satisfied by local consumption
#' @param product_aggr sum over products if TRUE
#' @param urb_aggr sum over products if TRUE
#' @param fvc_aggr sum over products if TRUE
#' @return share or level of food consumed locally, in disaggregated transport module
#' @author David M Chen
#' @importFrom luscale superAggregate
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' x <- localDemandShares(gdx)
#' }

localDemandShares <- function(gdx, type = "prod", level = "reg", product_aggr = TRUE, 
                                   urb_aggr = TRUE, fvc_aggr = TRUE, file = NULL) {

   localFoodConsumed <- readGDX(gdx, "ov40_dem_for_local",
                               select = list(type = "level"), react = "silent")


  if (type == "dem") {
   # food demand
   totalDemand <-readGDX(gdx, "i40_dem_food_cell", react = "silent")
   # split by fvc and restrict to k
   fvcFood <- readGDX(gdx, "i40_food_proc_demand")
   totalDemand <- add_dimension(totalDemand, dim = 3.3, add = "fvc", nm = c("trad" , "industr"))
   totalDemand <- totalDemand[, , getNames(fvcFood)]
   totalDemand[, , "trad"] <-  totalDemand[, , "trad"] * (1 - fvcFood[, getYears(totalDemand), ])
   totalDemand[, , "industr"] <- totalDemand[ , , "industr"] * fvcFood[, getYears(totalDemand), ]
   # calculate feed demand based on production of livstck and feed demand
    kli <- findset("kli") 
    fdB <-   readGDX(gdx, "im_feed_baskets")[,,list("kap" = "fish"), invert = TRUE]
    li <- readGDX(gdx, "ov_prod",  
       select = list(type = "level"), react = "silent")[,,kli]
    totalFeedDemand <- dimSums(li * fdB[,getYears(li),], dim = 3.1)
     # split by fvc and restrict to k 
   fvcFeed <- readGDX(gdx, "i40_feed_proc_demand")[, , getNames(fvcFood)]
   totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.3, add = "fvc", nm = c("trad" , "industr"))
   totalFeedDemand <- totalFeedDemand[, , getNames(fvcFood)]
   totalFeedDemand[, , "trad"] <-  totalFeedDemand[, , "trad"] * (1 - fvcFeed[, getYears(totalFeedDemand), ])
   totalFeedDemand[, , "industr"] <- totalFeedDemand[ , , "industr"] * fvcFeed[, getYears(totalFeedDemand), ]
   
   # add feed demand to rural demand
     totalDemand[,,"rural"] <- totalDemand[, , "rural"] + totalFeedDemand

  # if zero's add a small value to avoid division by zero, same as in weight later on
 totalDemand[totalDemand == 0] <- 1e-6

  # restrict localFoodConsumed to k
    localFoodConsumed <- localFoodConsumed[, , getNames(totalDemand, dim = 1)]
    

   if (product_aggr) { 
    totalDemand <- dimSums(totalDemand, dim = 3.1)
    localFoodConsumed <- dimSums(localFoodConsumed, dim = 3.1)
           }
    if (urb_aggr) { 
    totalDemand <- dimSums(totalDemand, dim = "urb")
    localFoodConsumed <- dimSums(localFoodConsumed, dim = "urb")
           }
   if (fvc_aggr) { 
    totalDemand <- dimSums(totalDemand, dim = "fvc")
    localFoodConsumed <- dimSums(localFoodConsumed, dim = "fvc")
           }


  shr <- localFoodConsumed / totalDemand 
  weight <- totalDemand
  shr[is.na(shr)] <- 1


   } else if (type == "prod"){

   pr <- production(gdx, products = "kcr", level = "cell")   
   prli <-  production(gdx, products = "kli", level = "cell")   

   #add foddr and pasture

   pr <- mbind(pr, prli)

   localFoodConsumed <- localFoodConsumed[, , getItems(pr, dim = 3)]
        
        #amount locally consumed
    shr <- dimSums(localFoodConsumed, 
                 dim = c(3.2,3.3)) / (pr + 1e-6)
   
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
