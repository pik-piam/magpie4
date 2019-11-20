#' @title productionRevenue
#' @description calcluates production revenue based on a MAgPIE gdx file.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr ggregate over products or not (boolean, default TRUE)
#' @return A MAgPIE object containing prodcution revenues.
#' @author Miodrag Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- productionRevenue(gdx)
#'   }

productionRevenue <- function(gdx, file=NULL, level="reg", products="kall", product_aggr=TRUE){
  
  q <- production(gdx, level="reg", products=products, product_aggr=FALSE, 
                  attributes="dm", water_aggr=TRUE)
  p <-     prices(gdx, level="reg", products=products, product_aggr=FALSE, 
                  attributes="dm", type="producer")
  
  
  out <- q*p
  
  if(product_aggr) out <- dimSums(out, dim=3) 
  if(level!="reg") out <- superAggregate(out, aggr_type="sum", level=level)
  out(out,file)
}