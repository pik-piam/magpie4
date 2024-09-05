#' @title CostTC
#' @description Reads data on TC costs
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing values related with overall value of production [million US$17]
#' @author David Chen
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- CostTC(gdx)
#'   }
#'

CostTC <- function(gdx,file=NULL,level="reg"){

  #Reads existing capital in each time step

  tc_cost <- readGDX(gdx,"ov_tech_cost", format = "first_found", select = list(type="level"))

  tc_cost <- dimSums(tc_cost, dim = 3)

  getNames(tc_cost) <- "TC Costs"

  if (level != "reg") tc_cost <- superAggregate(tc_cost, aggr_type = "sum", level = level)
  out <- tc_cost

  out(out,file)
}
