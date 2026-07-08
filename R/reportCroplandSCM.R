#' @title reportCroplandSCM
#' @description Reports cropland area under soil carbon management (SCM)
#'
#' @export
#'
#' @param gdx   GDX file
#' @param level aggregation level ("regglo" by default)
#' @return SCM area indicators as MAgPIE object (million ha)
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' x <- reportCroplandSCM(gdx)
#' }
#'
#' @section SCM area variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover\|Cropland\|Croparea under Soil Carbon Management | million ha | Total cropland area under soil carbon management
#' Resources\|Land Cover\|Cropland\|Croparea under Soil Carbon Management\|Share of Croparea | 1 | Share of cropland area under SCM
#' @md
reportCroplandSCM <- function(gdx, level = "regglo") {

  millionha <- " (million ha)"
  baseName  <- "Resources|Land Cover|Cropland|Croparea under Soil Carbon Management"

  # Actual SCM area
  scmArea <- croplandSCM(gdx, level = level, crop_aggr = TRUE)
  scmArea <- dimSums(scmArea, dim = 3)

  # Total cropland area for share calculation (consistent with target definition)
  # Uses vm_land("crop") which includes planted area + fallow
  croplandTotal <- land(gdx, types = "crop", level = level)
  croplandTotal <- dimSums(croplandTotal, dim = 3)

  # SCM share relative to total cropland (matches the target share setting)
  scmShare <- scmArea / croplandTotal
  scmShare[is.na(scmShare)] <- 0

  out <- mbind(
    setNames(scmArea, paste0(baseName, millionha)),
    setNames(scmShare, paste0(baseName, "|Share of Croparea (1)"))
  )

  return(out)
}
