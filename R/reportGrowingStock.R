#' @title reportGrowingStock
#' @description reports Growing stocks for woody materials
#'
#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @param indicator If the reported numbers are relative (mio m3/ha) or absolute (mio. m3). Default is relative.
#' @param detail if detail=FALSE, the subcategories of groups are not reported.
#' @return production as MAgPIE object. Unit: see names
#' @author Abhijeet Mishra
#' @examples
#'
#'   \dontrun{
#'     x <- reportGrowingStock(gdx)
#'   }
#'
#' @section Growing stock variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Growing Stock\|relative\|Forest | m3/ha | Relative growing stock in forests
#' Resources\|Growing Stock\|relative\|Plantations | m3/ha | Relative growing stock in plantations
#' Resources\|Growing Stock\|absolute\|Forest | Mm3 | Absolute growing stock in forests
#' Resources\|Growing Stock\|absolute\|Plantations | Mm3 | Absolute growing stock in plantations
#' @md

reportGrowingStock <- function(gdx, indicator = "relative", detail = FALSE, level = "regglo") {
  if (suppressWarnings(!is.null(readGDX(gdx, "fcostsALL")))) {
    x <- GrowingStock(gdx = gdx, level = level, indicator = indicator)
    if (indicator == "relative") {
      unit <- "(m3/ha)"
    } else if (indicator == "absolute") {
      unit <- "(Mm3)"
    }
    getNames(x) <- suppressWarnings(paste0("Resources|Growing Stock|", indicator, "|", reportingnames(getNames(x, dim = 1))))
    getNames(x) <- paste(getNames(x), unit, sep = " ")
    x <- summationhelper(x)
    return(x)
  } else {
    return(NULL)
  }
}
