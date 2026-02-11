#' @title PriceGHG
#' @description reads GHG emission prices out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param aggr aggregation used, currently only "weight" (weighted by population) (max is deprecated)
#' @return GHG emission prices as MAgPIE object (US$2017/tCO2, US$2017/tN2O, US$2017/tCH4)
#' @author Florian Humpenoeder, Amsalu W. Yalew
#' @seealso \code{\link{reportPriceGHG}}
#' @examples
#'
#'   \dontrun{
#'     x <- PriceGHG(gdx)
#'   }
#'

PriceGHG <- function(gdx, file = NULL, level = "reg", aggr = "weight") {
  reg <- readGDX(gdx, "im_pollutant_prices")
  reg <- reg[, readGDX(gdx, "t"), c("co2_c", "n2o_n_direct", "ch4")]

  reg[, , "co2_c"] <- reg[, , "co2_c"] * 12 / 44 #US$/tC -> US$/tCO2
  reg[, , "n2o_n_direct"] <- reg[, , "n2o_n_direct"] * 28 / 44 #US$/tN -> US$/tN2O

  stopifnot(aggr == "weight")

  weight <- reg
  weight[, , ] <- population(gdx, level = "reg")

  out(superAggregateX(reg, aggr_type = "weighted_mean", level = level, weight = weight),
      file)
}
