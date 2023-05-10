#' @title outputPerWorker
#' @description returns output per worker in crop+livestock production
#' @param gdx GDX file
#' @param level spatial aggregation to report employment ("reg", "glo", or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @return output per worker as magpie object
#' @author Debbora Leip
#' @export
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- outputPerWorker(gdx)
#' }

outputPerWorker <- function(gdx, level = "reg", file = NULL) {

  revenueCrops <- productionRevenue(gdx, level = level, products = "kcr", product_aggr = TRUE)
  revenueLivst <- productionRevenue(gdx, level = level, products = "kli", product_aggr = TRUE)
  revenue      <- revenueCrops + revenueLivst

  agEmpl <- readGDX(gdx, "ov36_employment", select = list(type = "level"), react = "silent")

  if (!is.null(agEmpl)) {
    agEmpl <- superAggregate(agEmpl, level = level, aggr_type = "sum")
    x <- revenue / agEmpl
  } else { # for MAgPIE versions before implementation of employment return NULL
    x <- NULL
  }

  out(x, file)

}
