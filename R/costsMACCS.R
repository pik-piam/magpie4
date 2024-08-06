#' @title costsMACCS
#' @description reads costs entering the objective function from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @return MAgPIE object containing mitigation costs [million US$05]
#' @author Debbora Leip
#' @importFrom magclass dimSums
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- costsMACCS(gdx)
#'   }
#'

costsMACCS <- function(gdx, file = NULL, level = "regglo") {

  maccsCosts <- readGDX(gdx, "ov_maccs_costs", react = "silent", select = list(type = "level"))

  if (is.null(getNames(maccsCosts))) {
    x <- NULL
  } else {
    x <- superAggregate(maccsCosts, aggr_type = "sum", level = level)
  }

  out(x, file)
}
