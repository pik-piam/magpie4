#' @title localDemandShare
#' @description returns labor and capital cost share out of factor costs (i.e. labor + capital)
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation to report employment ("reg", "glo" or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @param product_aggr sum over products if TRUE
#' @return share or level of food consumed locally, in disaggregated transport module
#' @author David M Chen
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- localDemandShare(gdx)
#' }

localDemandShare <- function(gdx, level = "reg", product_aggr = FALSE, file = NULL) {

  localDemand <- readGDX(gdx, "i40_dem_food_cell", react = "silent")

   # add local feed demand to rural localFeedDemand
      prod <- production(gdx = gdx, products = "kli", level = "cell" )
      fBask <- readGDX(gdx, "im_feed_baskets",
                    react = "silent")[, getYears(prod), list("kap" = getItems(prod, dim = 3))]
  localFeed <- dimSums(prod * fBask, dim = 3.1)

  # add local feed demand to rural localFeedDemand
  localDemand <- localDemand
  localDemand[, , "rural"] <- localDemand[, , "rural"] + localFeed


  #amount locally consumed
  localFoodConsumed <- readGDX(gdx, "ov40_dem_for_local",
                               select = list(type = "level"), react = "silent")

  if (product_aggr) {
    localFoodConsumed <- dimSums(localFoodConsumed, dim = 3.1)
    localDemand <- dimSums(localDemand, dim = 3.1)
  }
  share <- localFoodConsumed / localDemand[, , getItems(localFoodConsumed, dim = 3)]
  #make 0/0 NAs 1
  share[is.na(share)] <- 1



  if (level!="cell"){
   share <- superAggregate(share, aggr_type="weighted_mean",
                             weight = localDemand[, , getItems(share, dim = 3)] + 1e-5,
                             level = level)
  }

  out(share, file)
}
