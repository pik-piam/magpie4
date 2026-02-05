#' @title reportNitrogenEfficiencies
#' @description Reports different nitrogen use efficiency indicators
#'
#' @export
#'
#' @param gdx GDX file
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{reportNitrogenEfficiencies}}
#'
#' @examples
#' \dontrun{
#' x <- reportNitrogenEfficiencies(gdx)
#' }
#'
#' @section Nitrogen efficiency variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Nitrogen\|Cropland Budget\|Nitrogen Use Efficiency complete | Mt Nr/Mt Nr | Complete nitrogen use efficiency on cropland
#' Resources\|Nitrogen\|Cropland Budget\|Nitrogen Use Efficiency basic | Mt Nr/Mt Nr | Basic nitrogen use efficiency (harvest/inputs)
#' Resources\|Nitrogen\|Cropland Budget\|Soil Nitrogen Uptake Efficiency | Mt Nr/Mt Nr | Soil nitrogen uptake efficiency
#' Resources\|Nitrogen\|Pasture Budget\|Nitrogen Use Efficiency complete | Mt Nr/Mt Nr | Complete nitrogen use efficiency on pastures
#' @md

#'
reportNitrogenEfficiencies <- function(gdx, level = "regglo") {

  budget <- reportNitrogenBudgetCropland(gdx = gdx, include_emissions = FALSE, grid = FALSE, level = level)
  budget2 <- reportNitrogenBudgetPasture(gdx = gdx, include_emissions = FALSE, grid = FALSE, level = level)

  ### calculation of efficiency indicators

  nue <- clean_magpie(setNames(
    budget[, , "Resources|Nitrogen|Cropland Budget|Withdrawals (Mt Nr/yr)"] /
      (budget[, , "Resources|Nitrogen|Cropland Budget|Inputs (Mt Nr/yr)"] -
         budget[, , "Resources|Nitrogen|Cropland Budget|Balance|+|Soil Organic Matter (Mt Nr/yr)"]),
    "Resources|Nitrogen|Cropland Budget|Nitrogen Use Efficiency complete"
  ))

  basic_inputs <- c(
    "Resources|Nitrogen|Cropland Budget|Inputs|+|Biological Fixation Symbiotic Crops (Mt Nr/yr)",
    "Resources|Nitrogen|Cropland Budget|Inputs|+|Manure Recycled from Confinements (Mt Nr/yr)",
    "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer (Mt Nr/yr)",
    "Resources|Nitrogen|Cropland Budget|Inputs|+|Atmospheric Deposition (Mt Nr/yr)"
  )

  nueBasic <- clean_magpie(setNames(
    budget[, , "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Harvested Crops (Mt Nr/yr)"] /
      dimSums(budget[, , basic_inputs], dim = 3),
    "Resources|Nitrogen|Cropland Budget|Nitrogen Use Efficiency basic"
  ))

  snupe_internal <- c(
    "Resources|Nitrogen|Cropland Budget|Inputs|+|Biological Fixation Symbiotic Crops (Mt Nr/yr)",
    "Resources|Nitrogen|Cropland Budget|Inputs|+|Seed (Mt Nr/yr)"
  )
  snupe <- clean_magpie(setNames(
    (budget[, , "Resources|Nitrogen|Cropland Budget|Withdrawals (Mt Nr/yr)"]
     - dimSums(budget[, , snupe_internal], dim = 3)) /
      (budget[, , "Resources|Nitrogen|Cropland Budget|Inputs (Mt Nr/yr)"] -
         dimSums(budget[, , snupe_internal], dim = 3) -
         budget[, , "Resources|Nitrogen|Cropland Budget|Balance|+|Soil Organic Matter (Mt Nr/yr)"]),
    "Resources|Nitrogen|Cropland Budget|Soil Nitrogen Uptake Efficiency"
  ))

  nue_pasture <- clean_magpie(setNames(
    budget2[, , "Resources|Nitrogen|Pasture Budget|Withdrawals (Mt Nr/yr)"] /
      budget2[, , "Resources|Nitrogen|Pasture Budget|Inputs (Mt Nr/yr)"],
    "Resources|Nitrogen|Pasture Budget|Nitrogen Use Efficiency complete"
  ))

  out <- mbind(nue, nueBasic, snupe, nue_pasture)

  getNames(out) <- paste0(getNames(out), " (Mt Nr/Mt Nr)")

  return(out)
}
