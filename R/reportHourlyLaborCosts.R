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

reportHourlyLaborCosts <- function(gdx, level = "regglo") {

  out <- hourlyLaborCosts(gdx, level = level)

  if (!is.null(out)) {
    out <- setNames(out, "Labor|Wages|Hourly labor costs (USDMER05/h)")
  }

  return(out)
}
