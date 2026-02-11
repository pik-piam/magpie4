#' @title reportWaterAvailability
#' @description reports water availability
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @return water availability as MAgPIE object Unit: see names
#' @author Felicitas Beier
#' @examples
#'
#'   \dontrun{
#'     x <- reportWaterAvailability(gdx)
#'   }
#'
#' @section Water availability variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Water\|Availability\|Agriculture | km3/yr | Water available for agricultural use
#' @md


reportWaterAvailability <- function(gdx, level = "regglo") {
  x           <- water_avail(gdx, file = NULL, level = level, sources = NULL, sum = TRUE, digits = 3)
  getNames(x) <- "Resources|Water|Availability|Agriculture (km3/yr)"
  return(x)
}
