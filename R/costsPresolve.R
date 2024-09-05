#' @title costsPresolve
#' @description reads presovle costs (i.e. without bioenergy demand) entering the objective function from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing the goal function costs in presolve mode [million US$17]
#' @details Presolve is without bioenergy demand. Hence costs from a MAgPIE run with bioenergy demand minus costs from presolve reflect costs that can be attributed to bioenergy production
#' @author Florian Humpenoeder
#' @examples
#'   \dontrun{
#'     x <- costsPresolve(gdx)
#'   }

costsPresolve <- function(gdx,file=NULL,level="reg") {
  
  x <- readGDX(gdx,"o90_cost_reg", react="silent")
  if(!is.null(x)) x <- superAggregate(x, aggr_type = "sum", level = level)
  
  out(x,file)
}