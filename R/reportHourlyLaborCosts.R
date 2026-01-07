#' @title reportHourlyLaborCosts
#' @description reports hourly labor costs in agriculture from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @return hourly labor costs as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportHourlyLaborCosts(gdx)
#'   }
#'
#'
#' @section Hourly labor cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Labor\|Wages\|Hourly labor costs | US$2017/h | Hourly labor costs in agriculture
#' @md


reportHourlyLaborCosts <- function(gdx, level = "regglo") {

  out <- hourlyLaborCosts(gdx, level = level)

  if (!is.null(out)) {
    out <- setNames(out, "Labor|Wages|Hourly labor costs (US$2017/h)")
  }

  return(out)
}
