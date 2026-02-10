#' @title costsWholesale
#' @description Reads data to calculate wholesale costs
#' @export
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregateX
#' @return A MAgPIE object containing values related with costs wholesale trade [million US$17/tDM]
#' @author David M Chen
#' @examples
#' \dontrun{
#' x <- costsWholesale(gdx)
#' }
#'
costsWholesale <- function(gdx, file = NULL, level = "regglo") {

  #read packaging costs
  if (is.null(readGDX(gdx, "ov_cost_packaging", select = list(type = "level"), react = "silent"))) {
    x <- NULL
  } else {
    pkC <- readGDX(gdx, "ov_cost_packaging", select = list(type = "level"), react = "silent")
    x <- superAggregateX(pkC, aggr_type = "sum", level = level)
  }
  return(x)
}
