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
#' Resources\|Land Cover\|Cropland\|Croparea under Soil Carbon Management | million ha | SCM area on crop area (legacy name, identical to\|+\|On Croparea)
#' Resources\|Land Cover\|Cropland\|Croparea under Soil Carbon Management\|Share of Croparea | 1 | Share of the SCM area on crop area in total cropland area
#' Resources\|Land Cover\|Cropland\|Area under Soil Carbon Management | million ha | Total SCM area including fallow land
#' Resources\|Land Cover\|Cropland\|Area under Soil Carbon Management\|+\|On Croparea | million ha | SCM area on crop area
#' Resources\|Land Cover\|Cropland\|Area under Soil Carbon Management\|+\|On Fallow | million ha | SCM area on fallow land (0 for older MAgPIE versions)
#' Resources\|Land Cover\|Cropland\|Area under Soil Carbon Management\|Share of Cropland | 1 | Share of total SCM area in total cropland area (matches the SCM target)
#' Resources\|Land Cover\|Cropland\|Area under Soil Carbon Management\|Share of Fallow | 1 | Share of fallow land under SCM
#' @md
reportCroplandSCM <- function(gdx, level = "regglo") {

  millionha <- " (million ha)"
  baseName  <- "Resources|Land Cover|Cropland|Croparea under Soil Carbon Management"
  areaName  <- "Resources|Land Cover|Cropland|Area under Soil Carbon Management"

  # SCM area, split into crop area and fallow land
  # (fallow land is only reported by newer MAgPIE versions)
  scm <- croplandSCM(gdx, level = level, crop_aggr = TRUE, fallow = "separate")
  cropScm <- scm[, , "crop_scm"]

  if ("fallow_scm" %in% getNames(scm)) {
    fallowScm <- scm[, , "fallow_scm"]
  } else {
    fallowScm <- cropScm * 0
  }

  totalScm <- cropScm + fallowScm

  # Total cropland area for share calculation (consistent with target definition)
  # Uses vm_land("crop") which includes planted area + fallow
  croplandTotal <- land(gdx, types = "crop", level = level)
  croplandTotal <- dimSums(croplandTotal, dim = 3)

  # SCM shares relative to total cropland (the "Cropland" share matches the
  # target definition of the SCM target)
  cropShare <- cropScm / croplandTotal
  cropShare[is.na(cropShare)] <- 0
  totalShare <- totalScm / croplandTotal
  totalShare[is.na(totalShare)] <- 0

  # Share of fallow land under SCM
  fallowTotal <- readGDX(gdx, "ov_fallow", select = list(type = "level"))
  fallowTotal <- gdxAggregate(gdx = gdx, x = fallowTotal, weight = "land",
                              types = "crop", to = level, absolute = TRUE)
  fallowShare <- fallowScm / fallowTotal
  fallowShare[is.na(fallowShare)] <- 0

  out <- mbind(
    # legacy names, kept as long as magpie4 has to work with both MAgPIE versions
    setNames(cropScm, paste0(baseName, millionha)),
    setNames(cropShare, paste0(baseName, "|Share of Croparea (1)")),
    setNames(totalScm, paste0(areaName, millionha)),
    setNames(cropScm, paste0(areaName, "|+|On Croparea", millionha)),
    setNames(fallowScm, paste0(areaName, "|+|On Fallow", millionha)),
    setNames(totalShare, paste0(areaName, "|Share of Cropland (1)")),
    setNames(fallowShare, paste0(areaName, "|Share of Fallow (1)"))
  )

  return(out)
}
