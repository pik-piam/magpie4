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
reportLandConservation <- function(gdx) {

  # ------ Land conservation
  landConsv <- landConservation(gdx, level = "regglo", annualRestor = FALSE)

  conserved <- dimSums(landConsv, dim=3.2)
  protected <- collapseDim(landConsv[,, "protect"], dim = 3.2)
  restored <- collapseDim(landConsv[,, "restore"], dim = 3.2)

  # aggreate and rename
  x <- NULL
  x <- mbind(x, setNames(conserved[, , "crop"], paste0("Resources|Land Cover Conserved|", reportingnames("crop"), "(million ha)")))
  x <- mbind(x, setNames(conserved[, , "past"], paste0("Resources|Land Cover Conserved|", reportingnames("past"), "(million ha)")))
  x <- mbind(x, setNames(conserved[, , "forestry"], paste0("Resources|Land Cover Conserved|Forest|", reportingnames(getNames(a[, , "forestry"], dim = 1)), "(million ha)")))
  x <- mbind(x, setNames(conserved[, , "primforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("primforest"), "(million ha)")))
  x <- mbind(x, setNames(conserved[, , "secdforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("secdforest"), "(million ha)")))
  x <- mbind(x, setNames(conserved[, , "urban"], paste0("Resources|Land Cover Conserved|", reportingnames(getNames(a[, , "urban"], dim = 1)), "(million ha)")))
  x <- mbind(x, setNames(conserved[, , "other"], paste0("Resources|Land Cover Conserved|", reportingnames(getNames(a[, , "other"], dim = 1)), "(million ha)")))

  x <- mbind(x, setNames(protected[, , "crop"], paste0("Resources|Land Cover Conserved|", reportingnames("crop"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "past"], paste0("Resources|Land Cover Conserved|", reportingnames("past"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "forestry"], paste0("Resources|Land Cover Conserved|Forest|", reportingnames(getNames(a[, , "forestry"], dim = 1)), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "primforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("primforest"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "secdforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("secdforest"), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "urban"], paste0("Resources|Land Cover Conserved|", reportingnames(getNames(a[, , "urban"], dim = 1)), "|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "other"], paste0("Resources|Land Cover Conserved|", reportingnames(getNames(a[, , "other"], dim = 1)), "|+|Protected (million ha)")))

  x <- mbind(x, setNames(restored[, , "crop"], paste0("Resources|Land Cover Conserved|", reportingnames("crop"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "past"], paste0("Resources|Land Cover Conserved|", reportingnames("past"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "forestry"], paste0("Resources|Land Cover Conserved|Forest|", reportingnames(getNames(a[, , "forestry"], dim = 1)), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "primforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("primforest"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "secdforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("secdforest"), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "urban"], paste0("Resources|Land Cover Conserved|", reportingnames(getNames(a[, , "urban"], dim = 1)), "|+|Restored (million ha)")))
  x <- mbind(x, setNames(restored[, , "other"], paste0("Resources|Land Cover Conserved|", reportingnames(getNames(a[, , "other"], dim = 1)), "|+|Restored (million ha)")))

  # ------ Annual Restoration
  annRestored <- landConservation(gdx, level = "regglo", annualRestor = TRUE)

  x <- mbind(x, setNames(annRestored[, , "crop"], paste0("Resources|Land Cover Conserved|", reportingnames("crop"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "past"], paste0("Resources|Land Cover Conserved|", reportingnames("past"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "forestry"], paste0("Resources|Land Cover Conserved|Forest|", reportingnames(getNames(a[, , "forestry"], dim = 1)), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "primforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("primforest"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "secdforest"], paste0("Resources|Land Cover Conserved|Forest|Natural Forest|", reportingnames("secdforest"), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "urban"], paste0("Resources|Land Cover Conserved|", reportingnames(getNames(a[, , "urban"], dim = 1)), "|Restored annually (million ha/yr)")))
  x <- mbind(x, setNames(annRestored[, , "other"], paste0("Resources|Land Cover Conserved|", reportingnames(getNames(a[, , "other"], dim = 1)), "|Restored annually (million ha/yr)")))

  # ------ Cumulative Restoration
  cumRestored <- landConservation(gdx, level = "regglo", restorCumSum = TRUE, baseyear = 2025)
  cumRestored <- collapseDim(b[,, "restore"], dim = 3.2)

  x <- mbind(x, setNames(cumRestored[, , "crop"], paste0("Resources|Land Cover Conserved|Cumulative|", reportingnames("crop"), "|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "past"], paste0("Resources|Land Cover Conserved|Cumulative|", reportingnames("past"), "|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "forestry"], paste0("Resources|Land Cover Conserved|Cumulative|Forest|", reportingnames(getNames(a[, , "forestry"], dim = 1)), "|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "primforest"], paste0("Resources|Land Cover Conserved|Cumulative|Forest|Natural Forest|", reportingnames("primforest"), "|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "secdforest"], paste0("Resources|Land Cover Conserved|Cumulative|Forest|Natural Forest|", reportingnames("secdforest"), "|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "urban"], paste0("Resources|Land Cover Conserved|Cumulative|", reportingnames(getNames(a[, , "urban"], dim = 1)), "|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "other"], paste0("Resources|Land Cover Conserved|Cumulative|", reportingnames(getNames(a[, , "other"], dim = 1)), "|Restored (million ha)")))

  return(x)
}
