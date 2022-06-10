#' @title reportHourlyLaborCosts
#' @description reports hourly labor costs in agriculture from MAgPIE results
#' @param gdx GDX file
#' @return hourly labor costs as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportHourlyLaborCosts(gdx)
#'   }
#'

reportHourlyLaborCosts <- function(gdx) {

  out <- hourlyLaborCosts(gdx, level = "regglo")
  out <- setNames(out, "Hourly labor costs (USDMER05/h)")

  return(out)
}
