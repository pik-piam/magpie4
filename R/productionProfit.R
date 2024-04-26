#' @title productionProfit
#' @description calcluates aggregate producer profit based on a MAgPIE gdx file.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @return A MAgPIE object containing producers profit in million $US. 
#' @author Miodrag Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- productionProfit(gdx)
#'   }
#' 
#' @importFrom magclass as.magpie

productionProfit <- function(gdx, file=NULL, level="reg", dir="."){
  
  revenue <- productionRevenue(gdx, level="reg", products="kall", product_aggr=TRUE)
  cost    <- costs(gdx, level="reg", sum=FALSE)
  cost_set <- c("Input Factors","Land Conversion","Transport","TC",
                "N Fertilizer","P Fertilizer","GHG Emissions","MACCS","AEI",
                "Trade","Processing","Substitution processing")
  cost    <- dimSums(cost[,,cost_set],dim=3)
  
  x <- revenue - cost 
  
  out <- gdxAggregate(gdx,x,to=level,weight="land",type="crop",absolute = T,
                      dir = dir)
  

  out(out,file)
}