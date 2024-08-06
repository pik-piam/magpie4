#' @title       CostsAEI
#' @description reads AEI costs entering the objective function from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @return MAgPIE object containing costs for AEI [million US$05]
#'
#' @author Felicitas Beier
#'
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- CostsAEI(gdx)
#'   }
#'

CostsAEI <- function(gdx, file = NULL, level = "regglo"){

  AEI_costs <- readGDX(gdx,"ov_cost_AEI", format = "first_found", select = list(type = "level"))

  AEI_costs <- superAggregate(AEI_costs, aggr_type = "sum", level = level)

  out(AEI_costs, file)
}
