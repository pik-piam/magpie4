#' @title ConsumVal
#' @description Reads data to calculate overall consumption value
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing values related with consumption value
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- ConsumVal(gdx)
#'   }
#'

ConsumVal <- function(gdx,file=NULL,level="cell"){
  
  # Reads demand of materials and food for kall
  demand_kall_foodmat<-dimSums(demand(gdx,products="kall",attributes="dm",level=level)[,,c("food","other_util")],dim=3.1)
  
  #Read consumer prices
  prices_consum<- prices(gdx, level=level, products="kall", product_aggr=FALSE, attributes="dm", type="consumer")
  
  out<-dimSums(demand_kall_foodmat*prices_consum,dim=3)
  
  if (level != "cell") out <- superAggregate(out, aggr_type = "sum", level = level)
  
  
  out(out,file)
}
