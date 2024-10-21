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

  # read in regional data
  a <- landConservation(gdx, level = "regglo")

  conserved <- dimSums(a, dim=3.2)
  protected <- collapseDim(a[,, "protect"], dim = 3.2)
  restored <- collapseDim(a[,, "restore"], dim = 3.2)

  # aggreate and rename
  x <- NULL
  x <- mbind(x, setNames(conserved[, , "crop"], paste0("Resources|Land Cover|", reportingnames("crop"), "|Conserved (million ha)")))
  x <- mbind(x, setNames(conserved[, , "past"], paste0("Resources|Land Cover|", reportingnames("past"), "|Conserved (million ha)")))
  x <- mbind(x, setNames(conserved[, , "forestry"], paste0("Resources|Land Cover|Forest|", reportingnames(getNames(a[, , "forestry"], dim = 1)), "|Conserved (million ha)")))
  x <- mbind(x, setNames(conserved[, , "primforest"], paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("primforest"), "|Conserved (million ha)")))
  x <- mbind(x, setNames(conserved[, , "secdforest"], paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("secdforest"), "|Conserved (million ha)")))
  x <- mbind(x, setNames(conserved[, , "urban"], paste0("Resources|Land Cover|", reportingnames(getNames(a[, , "urban"], dim = 1)), "|Conserved (million ha)")))
  x <- mbind(x, setNames(conserved[, , "other"], paste0("Resources|Land Cover|", reportingnames(getNames(a[, , "other"], dim = 1)), "|Conserved (million ha)")))


  x <- mbind(x, setNames(protected[, , "crop"], paste0("Resources|Land Cover|", reportingnames("crop"), "|Conserved|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "past"], paste0("Resources|Land Cover|", reportingnames("past"), "|Conserved|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "forestry"], paste0("Resources|Land Cover|Forest|", reportingnames(getNames(a[, , "forestry"], dim = 1)), "|Conserved|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "primforest"], paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("primforest"), "|Conserved|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "secdforest"], paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("secdforest"), "|Conserved|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "urban"], paste0("Resources|Land Cover|", reportingnames(getNames(a[, , "urban"], dim = 1)), "|Conserved|+|Protected (million ha)")))
  x <- mbind(x, setNames(protected[, , "other"], paste0("Resources|Land Cover|", reportingnames(getNames(a[, , "other"], dim = 1)), "|Conserved|+|Protected (million ha)")))


  x <- mbind(x, setNames(restored[, , "crop"], paste0("Resources|Land Cover|", reportingnames("crop"), "|Conserved|+|Restored (million ha/yr)")))
  x <- mbind(x, setNames(restored[, , "past"], paste0("Resources|Land Cover|", reportingnames("past"), "|Conserved|+|Restored (million ha/yr)")))
  x <- mbind(x, setNames(restored[, , "forestry"], paste0("Resources|Land Cover|Forest|", reportingnames(getNames(a[, , "forestry"], dim = 1)), "|Conserved|+|Restored (million ha/yr)")))
  x <- mbind(x, setNames(restored[, , "primforest"], paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("primforest"), "|Conserved|+|Restored (million ha/yr)")))
  x <- mbind(x, setNames(restored[, , "secdforest"], paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("secdforest"), "|Conserved|+|Restored (million ha/yr)")))
  x <- mbind(x, setNames(restored[, , "urban"], paste0("Resources|Land Cover|", reportingnames(getNames(a[, , "urban"], dim = 1)), "|Conserved|+|Restored (million ha/yr)")))
  x <- mbind(x, setNames(restored[, , "other"], paste0("Resources|Land Cover|", reportingnames(getNames(a[, , "other"], dim = 1)), "|Conserved|+|Restored (million ha/yr)")))

  b <- landConservation(gdx, level = "regglo", restorCumSum = TRUE, baseyear = 2025)
  cumRestored <- collapseDim(b[,, "restore"], dim = 3.2)

  x <- mbind(x, setNames(cumRestored[, , "crop"], paste0("Resources|Land Cover|", reportingnames("crop"), "|Conserved|Cumulative|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "past"], paste0("Resources|Land Cover|", reportingnames("past"), "|Conserved|Cumulative|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "forestry"], paste0("Resources|Land Cover|Forest|", reportingnames(getNames(a[, , "forestry"], dim = 1)), "|Conserved|Cumulative|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "primforest"], paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("primforest"), "|Conserved|Cumulative|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "secdforest"], paste0("Resources|Land Cover|Forest|Natural Forest|", reportingnames("secdforest"), "|Conserved|Cumulative|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "urban"], paste0("Resources|Land Cover|", reportingnames(getNames(a[, , "urban"], dim = 1)), "|Conserved|Cumulative|Restored (million ha)")))
  x <- mbind(x, setNames(cumRestored[, , "other"], paste0("Resources|Land Cover|", reportingnames(getNames(a[, , "other"], dim = 1)), "|Conserved|Cumulative|Restored (million ha)")))

  return(x)
}
