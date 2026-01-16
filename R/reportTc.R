#' @title reportTc
#' @description reports Tc
#'
#' @export
#'
#' @param gdx GDX file
#' @param level Aggregation level of the returned Tc report
#' @return tc values as MAgPIE object (%/yr)
#' @author Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportTc(gdx)
#'   }
#'
#'
#' @section Technological change variables:
#' Name | Unit | Meta
#' ---|---|---
#' Productivity\|Yield-increasing technological change crops | %/yr | Annual rate of yield-increasing technological change for crops
#' @md


reportTc <- function(gdx, level = "regglo") {
  cr <- tc(gdx = gdx, level = level, type = "crop")
  getNames(cr) <- "Productivity|Yield-increasing technological change crops (%/yr)"
  out <- cr
  return(out)
}
