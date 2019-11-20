#' @title economicSurplus
#' @description calcluates aggregate producer surplus based on a MAgPIE gdx file.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param type surplus type: "producer" (default)
#' @return A MAgPIE object containing economic surpluses for consumers (to be included) or producers (depending on type)
#' @author Miodrag Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- priceIndex(gdx)
#'   }
#' 
#' @importFrom magclass as.magpie

economicSurplus <- function(gdx, file=NULL, level="reg", type="producer"){
  if(type=="producer"){
    revenue <- productionRevenue(gdx, level=level, products="kall", product_aggr=TRUE)
    cost    <- costs(gdx, level=level, sum=FALSE)
    cost_set <- c("Input Factors","Land Conversion","Transport","TC",
                  "N Fertilizer","P Fertilizer","GHG Emissions","MACCS","AEI",
                  "Trade","Processing","Substitution processing")
    cost    <- dimSums(cost[,,cost_set],dim=3)
    
    out <- revenue - cost 
  
    } else if(type=="consumer"){
    warning("Consumer surplus impementation is under development. Null returned.")
    out <- NULL   
    }
    
  out(out,file)
}