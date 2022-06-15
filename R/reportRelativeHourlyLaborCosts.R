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

reportRelativeHourlyLaborCosts  <- function(gdx) {

  out <- relativeHourlyLaborCosts(gdx, level = "regglo")

  if (!is.null(out)) {
    out <- setNames(out, "Labor costs per worker relative to GDP pc (%)")
  }

  return(out)
}
