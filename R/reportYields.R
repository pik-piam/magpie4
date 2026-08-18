#' @title reportYields
#' @description reports yields
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx      GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @param detail   if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param physical if true (default) physical area (croparea) used for yield calculation;
#'                 if false harvested area used for yield calculation
#' @return yield as MAgPIE object (t DM/ha)
#' @importFrom magpiesets reporthelper
#' @author Florian Humpenoeder, Xiaoxi Wang, Kristine Karstens, Abhijeet Mishra, Felicitas Beier
#' @examples
#' \dontrun{
#' x <- reportYields(gdx)
#' }
#'
#' @details Realized yields, weighted by the endogenous cropping pattern. The
#' `Productivity|Yields|Input data|*` variables hold the 1995 cropping pattern fixed instead,
#' so the ratio between the two families is not a clean yield gap.
#'
#' @section Yield by physical area variables:
#' Reported when `physical = TRUE` (the default). 22 variables at `detail = FALSE`, 76 at
#' `detail = TRUE`. The stem itself is not reported, and no name carries a `+` summation
#' marker - unlike `reportYieldsCropRaw` and `reportYieldsCropCalib`, which do emit them.
#'
#' Name | Unit | Meta
#' ---|---|---
#' Productivity\|Yields\|Yield by physical area\|Crops | t DM/ha | Yield of all crops
#' Productivity\|Yields\|Yield by physical area\|Crops\|Cereals | t DM/ha | Cereals (maize, rice, temperate cereals, tropical cereals)
#' Productivity\|Yields\|Yield by physical area\|Crops\|Oil crops | t DM/ha | Oil crops (cotton seed, groundnuts, oilpalms, other oil crops, soybean, sunflower)
#' Productivity\|Yields\|Yield by physical area\|Crops\|Sugar crops | t DM/ha | Sugar crops (sugar beet, sugar cane)
#' Productivity\|Yields\|Yield by physical area\|Crops\|Other crops | t DM/ha | Other crops (fruits, vegetables, nuts, potatoes, pulses, tropical roots)
#' Productivity\|Yields\|Yield by physical area\|Bioenergy crops | t DM/ha | Second-generation bioenergy crops
#' Productivity\|Yields\|Yield by physical area\|Forage | t DM/ha | Forage crops
#' Productivity\|Yields\|Yield by physical area\|Pasture | t DM/ha | Pasture biomass; no water split
#'
#' Every entry except Pasture is also reported split by water supply, as a lowercase leaf
#' (e.g. \|Crops\|irrigated) rather than a top-level branch. With `detail = TRUE` each group
#' additionally gains its individual crops on the same pattern.
#'
#' @section Yield by harvested area variables:
#' Reported when `physical = FALSE`. Same tree and counts, under the stem
#' Productivity\|Yields\|Yield by harvested area. Harvested area is physical area scaled by
#' the regional multicropping index `f18_multicropping`, so these yields differ from those
#' above by cropping intensity.
#' @md
reportYields <- function(gdx, detail = FALSE, physical = TRUE, level = "regglo") {

  if (physical) {
    indicatorName <- "Productivity|Yields|Yield by physical area"
  } else {
    indicatorName <- "Productivity|Yields|Yield by harvested area"
  }

  if (!(level %in% c("reg", "regglo", "glo") || isCustomAggregation(level))) {
    stop("reportYields does not support aggregation level: ", level)
  }

  yieldWaterAgg <- function(watAgg = TRUE) {

    prod <- production(gdx, level = level, products = readGDX(gdx, "kcr"),
                       product_aggr = FALSE, water_aggr = watAgg)
    prod <- reporthelper(x = prod, dim = 3.1, level_zero_name = indicatorName,
                         detail = detail)

    area <- croparea(gdx, level = level, products = readGDX(gdx, "kcr"),
                     product_aggr = FALSE, water_aggr = watAgg, physical = physical)
    area <- reporthelper(x = area, dim = 3.1, level_zero_name = indicatorName,
                         detail = detail)

    out <- ifelse(prod > 1e-10, prod / area, NA)
    getNames(out) <- paste(gsub("\\.", "|", getNames(out)), "(t DM/ha)", sep = " ")

    return(out)
  }

  x <- mbind(yieldWaterAgg(watAgg = TRUE),
             yieldWaterAgg(watAgg = FALSE))

  pasture <- yields(gdx, level = level, products = "pasture", attributes = "dm")
  pasture <- summationhelper(reporthelper(x = pasture, dim = 3.1,
                                          level_zero_name = indicatorName, detail = detail),
                             sep = NULL)
  getNames(pasture) <- paste(getNames(pasture), "(t DM/ha)", sep = " ")

  x <- mbind(x, pasture)

  return(x)
}
