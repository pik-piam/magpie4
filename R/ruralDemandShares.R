#' @title ruralDemandShares
#' @description reports rural demand shares based on local consumption
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation to report employment ("reg", "glo" or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @param type Type of ratio that should be calculated
#' \itemize{
#'        \item \code{all}: How much rural & trad demand as a share of all demand is satisfied locally
#'        \item \code{tradOnly}: How much rural & trad demand as a share of rural & trad demand is satisfied locally
#'        \item \code{potential}: How much total gridded demand is potentially
#'                                satisfied by gridded production
#'        }
#' @param product_aggr sum over products if TRUE
#' @return share of food consumed locally
#' @author David M Chen
#' @importFrom luscale superAggregate
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' x <- localDemandShares(gdx)
#' }

ruralDemandShares <- function(gdx, type = "tradOnly", level = "reg", product_aggr = TRUE, file = NULL) {

  if (type %in% c("all", "tradOnly")) {  #rural food as share of all rural food
    # food demand
    totalDemand <- readGDX(gdx, "i40_dem_food_cell", react = "silent")

    if (!is.null(totalDemand)) {

      totalDemand <- add_dimension(totalDemand, dim = 3.2, add = "use", nm = c("food"))

      # calculate feed demand based on production of livstck and feed demand
      kli <- findset("kli")
      fdB <-   readGDX(gdx, "im_feed_baskets")[, , list("kap" = "fish"), invert = TRUE]
      li <- readGDX(gdx, "ov_prod",
                    select = list(type = "level"), react = "silent")[, , kli]
      totalFeedDemand <- dimSums(li * fdB[, getYears(li), ], dim = 3.1)
      # split by fvc

      totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.2, add = "use", nm = c("feed"))

      # add feed demand to rural demand
      totalDemand <- mbind(totalDemand, totalFeedDemand)

      # if we want the trad rural demand, then multiply by the share
      if (type == "tradOnly") {
        fvc <- collapseNames(readGDX(gdx, "i40_proc_demand_shr", react = "silent"))
        totalDemand <-  totalDemand * (1 - fvc[, getYears(totalDemand), ])
      }

      # if zero's add a small value to avoid division by zero, same as in weight later on
      totalDemand[totalDemand == 0] <- 1e-6

      # get actual amount consumed
      localFoodConsumed <- readGDX(gdx, "ov40_local_demand",
                                   select = list(type = "level"), react = "silent")

      if (product_aggr) {
        totalDemand <- dimSums(totalDemand, dim = 3.1)
        localFoodConsumed <- dimSums(localFoodConsumed, dim = 3.1)
      }

      #sum across food and feed for demand
      totalDemand <- dimSums(totalDemand, dim = "use")

      shr <- localFoodConsumed / totalDemand
      weight <- totalDemand
      shr[is.na(shr)] <- 1

    } else {
      message("Local demand module not on. Look at potential type share instead.")
      return(NULL)
    }

  } else if (type == "potential") {

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
      attr <- collapseNames(readGDX(gdx, "fm_nutrition_attributes")[, getYears(kcal), "kcal"][, , getNames(kcal)])
      pop <- readGDX(gdx, "im_pop_grid")[, getYears(kcal), ]
      urbShr <- (readGDX(gdx, "im_pop_urban_grid")[, getYears(kcal)] / pop)[, getYears(kcal), ]
      getNames(urbShr) <- "urb"
      rur <- 1 - urbShr
      getNames(rur) <- "rural"
      urbShr <- mbind(urbShr, rur)

      grdDem <-  kcal * urbShr * pop * 365 / (attr * 1e6) # in kg, so divide by 1e6 to get Mt
      getSets(grdDem)[which(getSets(grdDem) == "data")] <- "urb"
      grdDem <- add_dimension(grdDem, dim = 3.3, add = "use", nm = c("food"))
      #add missing columns that are in kpr
      missing <- setdiff(findset("k"), getNames(grdDem, dim = 1))
      grdDem <- add_columns(grdDem, addnm = missing, dim = 3.1, fill = 0)

      kli <- findset("kli")
      fdB <-   readGDX(gdx, "im_feed_baskets")[, , list("kap" = "fish"), invert = TRUE]
      li <- production(gdx, level = "cell", products = "kli")
      totalFeedDemand <- dimSums(li * fdB[, getYears(li), ], dim = 3.1)
      # split by fvc and restrict to k
      totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.2, add = "urb", nm = c("urb", "rural"))
      totalFeedDemand <- add_dimension(totalFeedDemand, dim = 3.3, add = "use", nm = c("feed"))
      totalFeedDemand[, , "urb"] <- 0

      totDem <- mbind(grdDem, totalFeedDemand[, , getNames(grdDem, dim = 1)])

      cnames  <- intersect(getNames(pr), getNames(totDem, dim = 1))

      if (product_aggr) {
        totDem <- dimSums(totDem[, , cnames], dim = 3.1)
        pr <- dimSums(pr[, , cnames], dim = 3.1)

        totDem <-  round(dimSums(totDem,
                                 dim = c("urb", "use"), 4))

        shr <- pr / totDem
        weight <-  totDem

      } else {
        totDem <- round(dimSums(totDem[, , cnames],
                                dim = c("urb", "use"), 4))
        shr <- pr[, , cnames] / totDem
        weight <-  totDem
      }
    }  else {
      message("Seems like no gridded population in this (older?) magpie version")
      return(NULL)
    }

  }

  shr[is.na(shr)] <- 1
  shr[is.infinite(shr)] <- 1

  #set values above 1 to 1
  shr[which(shr > 1)] <- 1

  if (level != "cell" && !is.null(weight)) {
    shr <- superAggregate(shr, aggr_type = "weighted_mean",
                          weight = weight + 1e-9,
                          level = level)
  }

  out(shr, file)
}
