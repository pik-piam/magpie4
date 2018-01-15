#' @title tradeValue
#' @description Calculates the value of traded goods based on a gdx file 
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param type exports-imports ("net-exports"), gross imports ("imports") or gross exports ("exports"); only valid if relative=FALSE
#' @param glo_weight Decides the calculation of global prices. Weighting schemes are applied for estimation of global producer price. If \code{"export"} prices are calculated as average of regional exporters' prices, weighted by the export volumes. If \code{"production"} (default), prices are calculated as average of regional prices weighted by regional production. Alternatively, if \code{"free_trade"}, the global prices are directly taken from the shadow prices of the global trade constraint, and no averaging is performed.
#' @return A MAgPIE object containing the value of trade flows in Million of US dollars
#' @author Misko Stevanovic, Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- tradeValue(gdx)
#'   }
#'
#' @importFrom gdx expand.set

tradeValue <- function(gdx, file=NULL, level="reg", products="k_trade", product_aggr=FALSE, type="net-exports", glo_weight="export") {
  
  #global prices
  glo_p <- prices(gdx, level="glo", products=products, glo_weight=glo_weight)  

  #regional trade flows
  volume <- trade(gdx,level="reg", products=products, type=type)
  
  #value of trade flows
  out <- volume*glo_p
  
  #aggregate
  out <- superAggregate(out,level=level,aggr_type="sum",crop_aggr=product_aggr)
  
  out(out,file)
}
