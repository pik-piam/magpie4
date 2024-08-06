#' @title PriceGHG
#' @description reads GHG emission prices out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param aggr aggregation used for global value; "max" (maxium value accross regions) or "weight" (weighted by population)
#' @return GHG emission prices as MAgPIE object (US$2005/tCO2, US$2005/tN2O, US$2005/tCH4)
#' @author Florian Humpenoeder, Amsalu W. Yalew
#' @importFrom luscale superAggregate
#' @seealso \code{\link{reportPriceGHG}}
#' @examples
#'
#'   \dontrun{
#'     x <- PriceGHG(gdx)
#'   }
#'

PriceGHG <- function(gdx, file=NULL, level="reg", aggr="max") {
  reg <- readGDX(gdx,"im_pollutant_prices")
  reg <- reg[,readGDX(gdx,"t"),c("co2_c","n2o_n_direct","ch4")]
#  reg <- reg/0.967 #USD2004 -> USD2005
  reg[,,"co2_c"] <- reg[,,"co2_c"]*12/44 #US$/tC -> US$/tCO2
  reg[,,"n2o_n_direct"] <- reg[,,"n2o_n_direct"]*28/44 #US$/tN -> US$/tN2O
  if(aggr=="max") {
    glo <- as.magpie(apply(reg,2:3,max))
  } else if (aggr=="weight") {
    weight <- reg
    weight[,,] <- population(gdx,level="reg")
    glo <- superAggregate(reg,level="glo",weight=weight,aggr_type = "weighted_mean")
  }
  if(level == "reg") x <- reg
  else if (level == "glo") x <- glo
  else if (level == "regglo") x <- mbind(reg,glo)
  out(x,file)
}
