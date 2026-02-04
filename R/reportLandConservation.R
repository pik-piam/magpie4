#' @title reportLandConservation
#' @description reports land conservation areas
#'
#' @export
#'
#' @param gdx GDX file
#' @return land conservation area in Mha
#' @author Patrick v. Jeetze, Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportLandConservation(gdx)
#' }
#'
#' @section Total conserved land variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover Conserved\|Cropland | million ha | Total conserved cropland area (protected + restored)
#' Resources\|Land Cover Conserved\|Pastures and Rangelands | million ha | Total conserved pasture area (protected + restored)
#' Resources\|Land Cover Conserved\|Forest\|Planted Forest | million ha | Total conserved planted forest area (protected + restored)
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest | million ha | Total conserved primary forest area (protected + restored)
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest | million ha | Total conserved secondary forest area (protected + restored)
#' Resources\|Land Cover Conserved\|Urban Area | million ha | Total conserved urban area (protected + restored)
#' Resources\|Land Cover Conserved\|Other Land | million ha | Total conserved other natural land area (protected + restored)
#'
#' @section Protected land variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover Conserved\|Cropland\|+\|Protected | million ha | Protected cropland area
#' Resources\|Land Cover Conserved\|Pastures and Rangelands\|+\|Protected | million ha | Protected pasture area
#' Resources\|Land Cover Conserved\|Forest\|Planted Forest\|+\|Protected | million ha | Protected planted forest area
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest\|+\|Protected | million ha | Protected primary forest area
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest\|+\|Protected | million ha | Protected secondary forest area
#' Resources\|Land Cover Conserved\|Urban Area\|+\|Protected | million ha | Protected urban area
#' Resources\|Land Cover Conserved\|Other Land\|+\|Protected | million ha | Protected other natural land area
#'
#' @section Restored land variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover Conserved\|Cropland\|+\|Restored | million ha | Restored cropland area
#' Resources\|Land Cover Conserved\|Pastures and Rangelands\|+\|Restored | million ha | Restored pasture area
#' Resources\|Land Cover Conserved\|Forest\|Planted Forest\|+\|Restored | million ha | Restored planted forest area
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest\|+\|Restored | million ha | Restored primary forest area
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest\|+\|Restored | million ha | Restored secondary forest area
#' Resources\|Land Cover Conserved\|Urban Area\|+\|Restored | million ha | Restored urban area
#' Resources\|Land Cover Conserved\|Other Land\|+\|Restored | million ha | Restored other natural land area
#'
#' @section Annual restoration variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover Conserved\|Cropland\|Restored annually | million ha/yr | Annual cropland restoration rate
#' Resources\|Land Cover Conserved\|Pastures and Rangelands\|Restored annually | million ha/yr | Annual pasture restoration rate
#' Resources\|Land Cover Conserved\|Forest\|Planted Forest\|Restored annually | million ha/yr | Annual planted forest restoration rate
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest\|Restored annually | million ha/yr | Annual primary forest restoration rate
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest\|Restored annually | million ha/yr | Annual secondary forest restoration rate
#' Resources\|Land Cover Conserved\|Urban Area\|Restored annually | million ha/yr | Annual urban area restoration rate
#' Resources\|Land Cover Conserved\|Other Land\|Restored annually | million ha/yr | Annual other land restoration rate
#'
#' @section Cumulative restoration variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover Conserved\|Cropland\|Restored cumulatively | million ha since 2025 | Cumulative cropland restoration since 2025
#' Resources\|Land Cover Conserved\|Pastures and Rangelands\|Restored cumulatively | million ha since 2025 | Cumulative pasture restoration since 2025
#' Resources\|Land Cover Conserved\|Forest\|Planted Forest\|Restored cumulatively | million ha since 2025 | Cumulative planted forest restoration since 2025
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Primary Forest\|Restored cumulatively | million ha since 2025 | Cumulative primary forest restoration since 2025
#' Resources\|Land Cover Conserved\|Forest\|Natural Forest\|Secondary Forest\|Restored cumulatively | million ha since 2025 | Cumulative secondary forest restoration since 2025
#' Resources\|Land Cover Conserved\|Urban Area\|Restored cumulatively | million ha since 2025 | Cumulative urban area restoration since 2025
#' Resources\|Land Cover Conserved\|Other Land\|Restored cumulatively | million ha since 2025 | Cumulative other land restoration since 2025
#' @md

#'
reportLandConservation <- function(gdx, level = "regglo") {

  # ------ Land conservation
  landConsv <- landConservation(gdx, level = level, annualRestor = FALSE)

  conserved <- dimSums(landConsv, dim = 3.2)
  protected <- collapseDim(landConsv[, , "protect"], dim = 3.2)
  restored <- collapseDim(landConsv[, , "restore"], dim = 3.2)

  # aggreate and rename
  x <- NULL
  x <- mbind(x, setNames(conserved[, , "crop"], paste0("Resources|Land Cover Conserved|", reportingnames("crop"), " (million ha)")))
  x <- mbind(x, setNames(conserved[, , "past"], paste0("Resources|Land Cover Conserved|", reportingnames("past"), " (million ha)")))
  x <- mbind(x, setNames(conserved[, , "forestry"], paste0("Resources|Land Cover Conserved|Forest|", reportingnames("forestry"), " (million ha)")))
  x <- mbind(x, setNames(conserved[, , "primforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("primforest"), " (million ha)")))
  x <- mbind(x, setNames(conserved[, , "secdforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("secdforest"), " (million ha)")))
  x <- mbind(x, setNames(conserved[, , "urban"], paste0("Resources|Land Cover Conserved|", reportingnames("urban"), " (million ha)")))
  x <- mbind(x, setNames(conserved[, , "other"], paste0("Resources|Land Cover Conserved|", reportingnames("other"), " (million ha)")))

  x <- mbind(x, setNames(protected[, , "crop"], paste0("Resources|Land Cover Conserved|", reportingnames("crop"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "past"], paste0("Resources|Land Cover Conserved|", reportingnames("past"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "forestry"], paste0("Resources|Land Cover Conserved|Forest|", reportingnames("forestry"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "primforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("primforest"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "secdforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("secdforest"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "urban"], paste0("Resources|Land Cover Conserved|", reportingnames("urban"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "other"], paste0("Resources|Land Cover Conserved|", reportingnames("other"), "|+|Protected (million ha)")))

  x <- mbind(x, setNames(restored[, , "crop"], paste0("Resources|Land Cover Conserved|", reportingnames("crop"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "past"], paste0("Resources|Land Cover Conserved|", reportingnames("past"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "forestry"], paste0("Resources|Land Cover Conserved|Forest|", reportingnames("forestry"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "primforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("primforest"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "secdforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("secdforest"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "urban"], paste0("Resources|Land Cover Conserved|", reportingnames("urban"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "other"], paste0("Resources|Land Cover Conserved|", reportingnames("other"), "|+|Restored (million ha)")))

  # ------ Annual Restoration
  annRestored <- landConservation(gdx, level = "regglo", annualRestor = TRUE)[,,"restore"]

  x <- mbind(x, setNames(annRestored[, , "crop"], paste0("Resources|Land Cover Conserved|", reportingnames("crop"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "past"], paste0("Resources|Land Cover Conserved|", reportingnames("past"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "forestry"], paste0("Resources|Land Cover Conserved|Forest|", reportingnames("forestry"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "primforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("primforest"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "secdforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("secdforest"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "urban"], paste0("Resources|Land Cover Conserved|", reportingnames("urban"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "other"], paste0("Resources|Land Cover Conserved|", reportingnames("other"), "|Restored annually (million ha/yr)")))

  # ------ Cumulative Restoration
  cumRestored <- landConservation(gdx, level = "regglo", cumuRestor = TRUE, baseyear = 2025)
  cumRestored <- collapseDim(cumRestored[,, "restore"], dim = 3.2)

  x <- mbind(x, setNames(cumRestored[, , "crop"], paste0("Resources|Land Cover Conserved|", reportingnames("crop"), "|Restored cumulatively (million ha since 2025)")))
  x <- mbind(x, setNames(cumRestored[, , "past"], paste0("Resources|Land Cover Conserved|", reportingnames("past"), "|Restored cumulatively (million ha since 2025)")))
  x <- mbind(x, setNames(cumRestored[, , "forestry"], paste0("Resources|Land Cover Conserved|Forest|", reportingnames("forestry"), "|Restored cumulatively (million ha since 2025)")))
  x <- mbind(x, setNames(cumRestored[, , "primforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("primforest"), "|Restored cumulatively (million ha since 2025)")))
  x <- mbind(x, setNames(cumRestored[, , "secdforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("secdforest"), "|Restored cumulatively (million ha since 2025)")))
  x <- mbind(x, setNames(cumRestored[, , "urban"], paste0("Resources|Land Cover Conserved|", reportingnames("urban"), "|Restored cumulatively (million ha since 2025)")))
  x <- mbind(x, setNames(cumRestored[, , "other"], paste0("Resources|Land Cover Conserved|", reportingnames("other"), "|Restored cumulatively (million ha since 2025)")))

  return(x)
}
