#' @title feed
#' @description calculates feed demand by animal type out of a gdx file
#
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param detail if FALSE, only total feed demand per animal type is calculated
#' without details on the type of feed
#' @param nutrient The nutrient in which the results shall be calculated
#' @param balanceflow If true, feed includes the calibration balanceflow
#' @return feed demand by animal type as MAgPIE object (unit depends on selected nutrient attributes)
#' @author Isabelle Weindl
#' @importFrom magclass dimSums
#' @export
#' @examples
#'   \dontrun{
#'     x <- feed(gdx)
#'   }
#'
feed <- function(gdx, file = NULL, level = "reg", detail = TRUE, nutrient = "dm", balanceflow = TRUE) {

  if (balanceflow == TRUE) {
    feed <- readGDX(gdx = gdx, "ov_dem_feed", select = list(type = "level"))
  } else if (balanceflow == FALSE) {
    prod <- readGDX(gdx = gdx, "ov_prod_reg", select = list(type = "level"))[, , readGDX(gdx, "kap")]
    feedbasket <- readGDX(gdx = gdx, "im_feed_baskets")
    feed <- feedbasket[, getYears(prod), ] * prod
  } else {
    stop("balanceflow must be TRUE or FALSE")
  }
  feedTypes <- getNames(feed, dim = 2)

  att <- readGDX(gdx, "fm_attributes")[, , feedTypes][, , nutrient]
  feed <- collapseNames(feed * att)

  if (!detail) {
    feed <- dimSums(feed, dim = 3.2)
  }
  if (level != "reg") {
    feed <- superAggregateX(feed, aggr_type = "sum", level = level)
  }
  out(feed, file)
}
