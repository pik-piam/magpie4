#' @title CostOverall
#' @description Reads data to calculate capital stocks
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing values related with overall value of production [million US$05]
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- CostOverall(gdx)
#'   }
#'

CostOverall <- function(gdx,file=NULL,level="reg"){
  
  # Gross value of production (GVoP_magpie)
  overall_magpie_costs <- readGDX(gdx,"ov11_cost_reg",react="silent",format="first_found", select = list(type="level"))
  trade_magpie_costs <- readGDX(gdx,"ov_cost_trade",react="silent",format="first_found", select = list(type="level"))
  processing_costs <- readGDX(gdx,"ov_cost_processing",react="silent",format="first_found", select = list(type="level"))
  
  GVoP_magpie<- overall_magpie_costs - trade_magpie_costs - processing_costs
  getNames(GVoP_magpie) <- "Gross value of production"
  
  if (level != "reg") GVoP_magpie <- superAggregate(GVoP_magpie, aggr_type = "sum", level = level)
  
  
  out(GVoP_magpie,file)
}
