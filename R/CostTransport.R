#' @title CostTransport
#' @description reads costs entering the objective function from a MAgPIE gdx file
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate.
#' @param sum total costs (TRUE) or detailed costs (FALSE)
#' @return A MAgPIE object containing the transport costs [million US$17]
#' @author David Chen
#' @importFrom magclass dimSums
#' @importFrom luscale superAggregate
#' @examples
#'   \dontrun{
#'     x <- CostTransport(gdx)
#'   }
#' @export

CostTransport <- function(gdx, file = NULL, level = "cell", sum = FALSE) {

  #delete Mainsolve also in magpie4 costs
  transport <-readGDX(gdx, "ov_cost_transp", react = "silent", format = "first_found", select = list(type = "level"))

  if (sum) {
    transport <- dimSums(transport, dim = 3.1)
    dimnames(transport)[[3]] <- "Transport"
  }

  if (level != "cell") {
    transport <- superAggregate(transport, aggr_type = "sum", level = level)
  }


  out(transport, file)
}
