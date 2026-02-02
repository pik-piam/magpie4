#' @title reportLandUseChange
#' @description reports land-use change
#'
#' @export
#'
#' @param gdx GDX file
#' @param baseyear baseyear for calculating land-use change
#' @return land-use change as MAgPIE object (million ha wrt to baseyear)
#' @author Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportLandUseChange(gdx)
#'   }
#'
#' @section Land-use change variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover Change\|Cropland | million ha wrt baseyear | Change in cropland area relative to baseyear
#' Resources\|Land Cover Change\|Pastures and Rangelands | million ha wrt baseyear | Change in pasture area relative to baseyear
#' Resources\|Land Cover Change\|Forest | million ha wrt baseyear | Change in forest area relative to baseyear
#' Resources\|Land Cover Change\|Other Land | million ha wrt baseyear | Change in other land area relative to baseyear
#' @md
reportLandUseChange <- function(gdx, baseyear = 1995, level = "regglo") {

  #get LandUse
  x <- reportLandUse(gdx, level = level)

  #drop variables
  x <- x[, , "Resources|Land Cover (million ha)", invert = TRUE]

  #calc land-use change wrt to baseyear
  x <- x - setYears(x[, baseyear, ], NULL)

  #rename variable and unit
  getNames(x) <- gsub("\\|Land Cover\\|", "\\|Land Cover Change\\|", getNames(x))
  getNames(x) <- gsub("\\(million ha\\)", paste0("\\(million ha wrt ", baseyear, "\\)"), getNames(x))

  return(x)
}
