#' @title reportCarbonstock
#' @description Reports the carbon stocks for future MAgPIE projections
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @param legacyEmis Logical (default TRUE). If TRUE, add the legacy-clearing slash/deadwood pool
#' (\code{\link{legacyEmissions}}) as an additive \code{+} child of \code{Resources|Carbon}, so the reported total
#' carbon includes the carbon still held as slash/deadwood under the reporting reframe. This mirrors the
#' \code{+|Legacy clearing} child on the emission side in \code{\link{reportEmissions}} and keeps emissions and
#' stocks consistent (the reframed Land-use Change flux equals minus the change in this pool). The soil/litter/
#' vegetation sub-pools stay model-native; only the aggregate changes (~0.5 percent). legacyEmis=FALSE =>
#' \code{Resources|Carbon} is the model's soil+litter+vegetation (backward compatible).
#' @author Kristine Karstens, Florian Humpenoeder
#' @examples
#'   \dontrun{
#'     x <- reportSOM(gdx)
#'   }
#'
#' @section Carbon stock variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Carbon | Mt C | Total terrestrial carbon stocks (incl. the legacy-clearing pool when legacyEmis=TRUE)
#' Resources\|Carbon\|+\|Soil | Mt C | Soil carbon stocks
#' Resources\|Carbon\|+\|Litter | Mt C | Litter carbon stocks
#' Resources\|Carbon\|+\|Vegetation | Mt C | Vegetation carbon stocks (above and below ground biomass)
#' Resources\|Carbon\|+\|Legacy clearing pool | Mt C | Slash/deadwood pool of the legacy-clearing reframe (additive child; only when legacyEmis=TRUE)
#' @md
reportCarbonstock <- function(gdx, level = "regglo", legacyEmis = TRUE) {

  x <- carbonstock(gdx, level = level, sum_cpool = FALSE, sum_land = TRUE)

  # Additive + children of Resources|Carbon (the model-native sub-pools).
  out <- mbind(
    setNames(x[, , "soilc"], "Resources|Carbon|+|Soil (Mt C)"),
    setNames(x[, , "litc"],  "Resources|Carbon|+|Litter (Mt C)"),
    setNames(x[, , "vegc"],  "Resources|Carbon|+|Vegetation (Mt C)")
  )

  # Legacy-clearing slash/deadwood pool (reporting reframe, cf. reportEmissions legacyEmis): a fourth additive
  # + child, so the reported Resources|Carbon total includes the carbon still held as slash/deadwood under the
  # reframe. Its change reconciles with the reframed Land-use Change flux (emission = minus change in stock),
  # mirroring the +|Legacy clearing child on the emission side. The soil/litter/vegetation sub-pools stay
  # model-native; only the aggregate changes. Built as a slice of `out` so it inherits the exact dim structure
  # (avoids an mbind subdimension-mismatch warning), then the value is overwritten.
  if (legacyEmis) {
    pool <- collapseNames(legacyEmissions(gdx, level = level, unit = "element")[, , "legacy_stock"])
    pool <- pool[getItems(out, dim = 1), getYears(out), ]   # align to out's region/year order
    poolLine <- setNames(out[, , "Resources|Carbon|+|Vegetation (Mt C)"],
                         "Resources|Carbon|+|Legacy clearing pool (Mt C)")
    poolLine[, , ] <- pool
    out <- mbind(out, poolLine)
  }

  # Resources|Carbon total = sum of its + children (three model-native pools, plus legacy pool when on).
  mbind(setNames(dimSums(out, dim = 3), "Resources|Carbon (Mt C)"), out)
}
