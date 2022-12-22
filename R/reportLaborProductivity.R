#' @title reportLaborProductivity
#' @description reports labor productivity in crop production
#'
#' @export
#'
#' @param gdx GDX file
#' @param productAggr Aggregate over products or not (boolean)
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return labor productivity as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportLaborProductivity(gdx)
#'   }
#'

reportLaborProductivity <- function(gdx, productAggr = TRUE, level = "regglo") {

  out <- laborProductivity(gdx, level = level, productAggr = TRUE)

  if (is.null(out)) return(NULL)

  out <- setNames(out, "Labor productivity|Crop products (kg DM per hour)")

  if (isFALSE(productAggr)) {
    laborProd <- laborProductivity(gdx, level = level, productAggr = FALSE)
    getNames(laborProd) <- paste0("Labor productivity|Crop products|",
                                   reportingnames(getNames(laborProd)), " (kg DM per hour)")
    out <- mbind(out, laborProd)
  }

  return(out)
}
