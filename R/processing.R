#' @title processing
#' @description Calculates MAgPIE disaggregated processing out of a gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param indicator process or secondary product output
#' @details Demand definitions are equivalent to FAO CBS categories
#' @return processing as MAgPIE object (Unit depends on attributes)
#' @author David Chen, Benjamin Leon Bodirsky
#' @importFrom magclass add_dimension dimOrder
#' @examples
#'
#'   \dontrun{
#'     x <- processing(gdx = gdx, level="regglo", products="kcr", indicator="primary_to_process")
#'   }
#'
processing <- function(gdx, level = "reg",
                       indicator = "secondary_from_primary") {

  if (indicator == "primary_to_process") {

    #primary into process
    processdemand <- readGDX(gdx = gdx, "ov20_dem_processing", select = list(type = "level"))

    out <- dimOrder(processdemand, c(1, 2))
    glo <- setItems(dimSums(out, dim = 1), dim = 1, "GLO")
    out <- mbind(out, glo)
  } else if (indicator == "secondary_from_primary") {
    # secondary from primary
    processdemand <- readGDX(gdx = gdx, "ov20_dem_processing", select = list(type = "level"))

    convFactors <- readGDX(gdx = gdx, "f20_processing_conversion_factors")

    years <- getYears(processdemand)
    convFactors <- convFactors[, years, ]

    out <- processdemand[, , getItems(convFactors, dim = 3.1)] * convFactors
    out <- dimSums(out, dim = 3.1)
    glo <- setItems(dimSums(out, dim = 1), dim = 1, "GLO")
    out <- mbind(out, glo)
  } else {
    stop("unknown indicator")
  }
  return(out)
}
