#' @title reportSDG9
#' @description reports all SDG indicators relevant for SD9 - Industrial innovation and infrastructure
#'
#' @export
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return MAgPIE object
#' @author Felicitas Beier
#' @examples
#'
#'   \dontrun{
#'     x <- reportSDG9(gdx)
#'   }
#'


reportSDG9 <- function(gdx, level = "regglo") {
  x <- NULL

  indicatorname <- "SDG|SDG9|Manufacturing value added"
  unit <- "percentage"
  #missing

  indicatorname <- "SDG|SDG9|CO2 industry intensity"
  unit <- "ton/2005USD"
  #missing

  indicatorname <- "SDG|SDG9|Investment in AgR&D"
  unit <- "USD05"
  #missing

  return(x)
}
