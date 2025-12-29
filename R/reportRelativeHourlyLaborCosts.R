#' @title reportRelativeHourlyLaborCosts
#' @description reports labor costs per ag. worker in relation to GDP pc from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @return labor costs per ag. worker in relation to GDP pc as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportRelativeHourlyLaborCosts(gdx)
#'   }
#'
#'
#' @section Relative hourly labor cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Labor\|Wages\|Labor costs per worker relative to GDP pc | % | Agricultural labor costs relative to GDP per capita
#' @md


reportRelativeHourlyLaborCosts  <- function(gdx) {

  out <- relativeHourlyLaborCosts(gdx, level = "regglo")

  if (!is.null(out)) {
    out <- setNames(out, "Labor|Wages|Labor costs per worker relative to GDP pc (%)")
  }

  return(out)
}
