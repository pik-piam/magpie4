#' @title reportLaborProductivity
#' @description reports labor productivity in crop production
#'
#' @export
#'
#' @param gdx GDX file
#' @param productAggr Aggregate over products or not (boolean)
#' @param type type of labor productivity, so far only physical (kg DM / h)
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return labor productivity as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportLaborProductivity(gdx)
#'   }
#'

reportLaborProductivity <- function(gdx, productAggr = TRUE, type = "physical", level = "regglo") {

  if (type == "pyhsical") {  
    out <- laborProductivity(gdx, level = level, productAggr = TRUE)

    if (is.null(out)) return(NULL)

    out <- setNames(out, "Labor|Productivity|Physical labor productivity|Crop products (kg DM per hour)")

    if (isFALSE(productAggr)) {
      laborProd <- laborProductivity(gdx, level = level, productAggr = FALSE)
      getNames(laborProd) <- paste0("Labor|Productivity|Physical labor productivity|Crop products|",
                                    reportingnames(getNames(laborProd)), " (kg DM per hour)")
      out <- mbind(out, laborProd)
    }
  } else {
    stop("Other types of labor productivity not implemented yet")
  }


  return(out)
}
