#' @title localDemandShares
#' @description reports local demand and production shares based on local consumption
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation to report employment ("reg", "glo" or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @param type Type of ratio that should be calculated
#' \itemize{
#'        \item \code{local}: How much local demand (taking into account industrial/rural split) is satisfied by local consumption
#'        \item \code{localtotal}: How much total gridded demand is satisfied by local consumption
#'        \item \code{potential}: How much total gridded demand is potentially
#'                                satisfied by gridded production
#'        \item \code{prod}: Not really used, to delete
#'        }
#' @param product_aggr sum over products if TRUE
#' @param urb_aggr sum over products if TRUE
#' @param fvc_aggr sum over products if TRUE
#' @param feed_aggr sum over food and feed if TRUE, buggy, keep TRUE for now
#' @return share or level of food consumed locally, in disaggregated transport module
#' @author David M Chen
#' @importFrom luscale superAggregate
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' x <- localDemandShares(gdx)
#' }

localDemandShares <- function(gdx, type = "local", level = "reg", product_aggr = TRUE, 
                                   urb_aggr = TRUE, fvc_aggr = TRUE, feed_aggr = TRUE, file = NULL) {

  if (type == "local") {
   # food demand
   totalDemand <-readGDX(gdx, "i72_dem_food_cell", react = "silent")

   if (!is.null(totalDemand)) { 

   # split by fvc and restrict to k
   fvcFood <- readGDX(gdx, "i72_food_proc_demand", react = "silent")
   totalDemand <- add_dimension(totalDemand, dim = 3.3, add = "fvc", nm = c("trad" , "industr"))
   totalDemand <- totalDemand[, , getNames(fvcFood)]
   totalDemand[, , "trad"] <-  totalDemand[, , "trad"] * (1 - fvcFood[, getYears(totalDemand), ])
   totalDemand[, , "industr"] <- totalDemand[ , , "industr"] * fvcFood[, getYears(totalDemand), ]
   totalDemand <- add_dimension(totalDemand, dim = 3.4, add = "use", nm = c("food"))
   
   # calculate feed demand based on production of livstck and feed demand
    kli <- findset("kli") 
    fdB <-   readGDX(gdx, "im_feed_baskets")[,,list("kap" = "fish"), invert = TRUE]
    li <- readGDX(gdx, "ov_prod",  
       select = list(type = "level"), react = "silent")[,,kli]
    totalFeedDemand <- dimSums(li * fdB[,getYears(li),], dim = 3.1)
     # split by fvc and restrict to k 
   fvcFeed <- readGDX(gdx, "i72_feed_proc_demand")[, , getNames(fvcFood)]
   totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.3, add = "fvc", nm = c("trad" , "industr"))
   totalFeedDemand <- totalFeedDemand[, , getNames(fvcFood)]
   totalFeedDemand[, , "trad"] <-  totalFeedDemand[, , "trad"] * (1 - fvcFeed[, getYears(totalFeedDemand), ])
   totalFeedDemand[, , "industr"] <- totalFeedDemand[ , , "industr"] * fvcFeed[, getYears(totalFeedDemand), ]
   totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.2, add = "urb", nm = c("urban", "rural"))
   totalFeedDemand[,,"urban"] <- 0
   totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.4, add = "use", nm = c("feed"))

   # add feed demand to rural demand
   totalDemand <- mbind(totalDemand, totalFeedDemand)

  # if zero's add a small value to avoid division by zero, same as in weight later on
   totalDemand[totalDemand == 0] <- 1e-6

 # get actual amount consumed
   localFoodConsumed <- readGDX(gdx, "ov72_dem_for_local",
                               select = list(type = "level"), react = "silent")

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

   if (feed_aggr) { 
    totalDemand <- dimSums(totalDemand, dim = "use")
   # localFoodConsumed <- dimSums(localFoodConsumed, dim = "use")
           }


  shr <- localFoodConsumed / totalDemand 
  weight <- totalDemand
  shr[is.na(shr)] <- 1
   } else {
      message("Local demand module not on. Look at potential type share instead.")
     return(NULL)
      }

   } else if (type == "localtotal") {
   # food demand
   totalDemand <-readGDX(gdx, "i72_dem_food_cell", react = "silent")
  
   if (!is.null(totalDemand)) { 
   fvcFood <- readGDX(gdx, "i72_food_proc_demand", react = "silent")
   totalDemand <- totalDemand[, , getNames(fvcFood)]
   totalDemand <- add_dimension(totalDemand, dim = 3.3, add = "use", nm = c("food"))

   # calculate feed demand based on production of livstck and feed demand
    kli <- findset("kli") 
    fdB <-   readGDX(gdx, "im_feed_baskets")[,,list("kap" = "fish"), invert = TRUE]
    li <- readGDX(gdx, "ov_prod",  
       select = list(type = "level"), react = "silent")[,,kli]
    totalFeedDemand <- dimSums(li * fdB[,getYears(li),], dim = 3.1)
  
    #restrict to k, get dimensions
    totalFeedDemand <- totalFeedDemand[, , getNames(fvcFood)]
    totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.2, add = "urb", nm = c("urban", "rural"))
    totalFeedDemand[,,"urban"] <- 0
    totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.3, add = "use", nm = c("feed"))

  
   # add feed demand to rural demand
   totalDemand <- mbind(totalDemand, totalFeedDemand)
   # if zero's add a small value to avoid division by zero, same as in weight later on
   totalDemand[totalDemand == 0] <- 1e-6

   # get actual amount consumed
   localFoodConsumed <- readGDX(gdx, "ov72_dem_for_local",
                               select = list(type = "level"), react = "silent")
   # restrict localFoodConsumed to k
    localFoodConsumed <- localFoodConsumed[, , getNames(totalDemand, dim = 1)]
  # sum over trad/industr because industr is 0 anyways
  localFoodConsumed <- dimSums(localFoodConsumed, dim = 3.3)

   if (product_aggr) { 
    totalDemand <- dimSums(totalDemand, dim = 3.1)
    localFoodConsumed <- dimSums(localFoodConsumed, dim = 3.1)
           }

     if (urb_aggr) { 
    totalDemand <- dimSums(totalDemand, dim = "urb")
    localFoodConsumed <- dimSums(localFoodConsumed, dim = "urb")
           }
 
   if (feed_aggr) { 
    totalDemand <- dimSums(totalDemand, dim = "use")
   # localFoodConsumed <- dimSums(localFoodConsumed, dim = "use")
          }


  shr <- localFoodConsumed / totalDemand 
  weight <- totalDemand
  shr[is.na(shr)] <- 1
   } else {
      message("Local demand module not on. Look at potential type share instead.")
     return(NULL)
      }

   } else if (type == "potential"){

  #calc production

  pop <- readGDX(gdx, "im_pop_grid", react = "silent")
  
  if (!is.null(pop)) {

  prod <- production(gdx, level = "cell", products = "kcr")
  prodli <- production(gdx, level = "cell", products = "kli")
  prpast <- production(gdx, products = "pasture", level = "cell")   
  #fodd <- production(gdx, products = "kres", level = "cell")      
   pr <- mbind(prod, prodli)
   pr <- mbind(pr, prpast)
    # need fodder still

   # recalculate gridded demand 
   kcal <- Kcal(gdx, level = "reg", product_aggr = FALSE, magpie_input = TRUE)
   attr <- collapseNames(readGDX(gdx, "fm_nutrition_attributes")[,getYears(kcal),"kcal"][,,getNames(kcal)])
   pop <- readGDX(gdx, "im_pop_grid")[, getYears(kcal), ]
   urbShr <- (readGDX(gdx, "im_pop_urban_grid")[,getYears(kcal)]/pop)[, getYears(kcal), ]
   getNames(urbShr) <- "urb"
   rur <- 1 - urbShr
   getNames(rur) <- "rural"
   urbShr <- mbind(urbShr, rur)

   grdDem <-  kcal * urbShr * pop * 365 / (attr * 1e6) # in kg, so divide by 1e6 to get Mt
   getSets(grdDem)[which(getSets(grdDem) == "data")] <- "urb"
   grdDem <- add_dimension(grdDem, dim = 3.3, add = "use", nm = c("food"))
   #add missing columns that are in kpr
   missing <- setdiff(findset("k"), getNames(grdDem, dim = 1))
   grdDem <- add_columns(grdDem, addnm = missing, dim = 3.1, fill = 0 ) 

   kli <- findset("kli") 
    fdB <-   readGDX(gdx, "im_feed_baskets")[,,list("kap" = "fish"), invert = TRUE]
    li <- production(gdx, level = "cell", products = "kli")
    totalFeedDemand <- dimSums(li * fdB[,getYears(li),], dim = 3.1)
     # split by fvc and restrict to k 
   totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.2, add = "urb", nm = c("urb" , "rural"))
   totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.3, add = "use", nm = c("feed"))
   totalFeedDemand[,,"urb"] <- 0

   totDem <- mbind(grdDem, totalFeedDemand[,,getNames(grdDem, dim = 1)])

   cnames  <- intersect(getNames(pr), getNames(totDem, dim = 1))

   if (product_aggr) { 
    totDem <- dimSums(totDem[,,cnames], dim = 3.1)
    pr <- dimSums(pr[,,cnames], dim = 3.1)
   
   totDem <-  round(dimSums(totDem, 
                 dim = c("urb", "use"), 4))

   shr <- pr / totDem 
   weight <-  totDem

  } else {
      message("Seems like no gridded population in this (older?) magpie version")
     return(NULL)
        }

 } else {

  totDem <- round(dimSums(totDem[, , cnames], 
                 dim = c("urb", "use"), 4)) 
   shr <- pr[, , cnames] / totDem
   weight <-  totDem

           }

   } else if (type == "prod"){

   pr <- production(gdx, products = "kcr", level = "cell")   
   prli <-  production(gdx, products = "kli", level = "cell")   
   prpast <- production(gdx, products = "pasture", level = "cell")   
  #fodd <- production(gdx, products = "kres", level = "cell")      

   pr <- mbind(pr, prli)
   pr <- mbind(pr, prpast)

   
 # get actual amount consumed
   localFoodConsumed <- readGDX(gdx, "ov72_dem_for_local",
                               select = list(type = "level"), react = "silent")
  if (is.null(localFoodConsumed)) { return (NULL)}

   localFoodConsumed <- localFoodConsumed[, , getItems(pr, dim = 3)]
        
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

 shr[is.na(shr)] <- 1
 shr[is.infinite(shr)] <- 1

 #set values above 1 to 1 
 shr[which(shr > 1 )] <- 1
    
    
 if (level!="cell"){
   if(!is.null(weight)){
   shr <- superAggregate(shr, aggr_type="weighted_mean",
                             weight = weight + 1e-9,
                             level = level)
  }}

  out(shr, file)
}

