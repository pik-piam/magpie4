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
#' Name | Unit | Meta
#' ---|---|---
#' Productivity\|Yields\|Yield by physical area | t DM/ha | Crop yields calculated as production divided by physical cropland area (fallow excluded)
#' Productivity\|Yields\|Yield by physical area\|+\|Crops | t DM/ha | Yield of all crops
#' Productivity\|Yields\|Yield by physical area\|Crops\|+\|Cereals | t DM/ha | Yield of cereals (maize, rice, temperate cereals and tropical cereals)
#' Productivity\|Yields\|Yield by physical area\|Crops\|+\|Oil crops | t DM/ha | Yield of oil crops (cotton seed, groundnuts, oilpalms, other oil crops, soybean, sunflower)
#' Productivity\|Yields\|Yield by physical area\|Crops\|+\|Sugar crops | t DM/ha | Yield of sugar crops (sugar beet, sugar cane)
#' Productivity\|Yields\|Yield by physical area\|Crops\|+\|Other crops | t DM/ha | Yield of other crops (fruits, vegetables, nuts, potatoes, pulses, tropical roots)
#' Productivity\|Yields\|Yield by physical area\|+\|Pasture | t DM/ha | Yield of pasture biomass
#' Productivity\|Yields\|Yield by physical area\|++\|Irrigated | t DM/ha | Yield on irrigated cropland
#' Productivity\|Yields\|Yield by physical area\|++\|Rainfed | t DM/ha | Yield on rainfed cropland
#'
#' @section Yield by harvested area variables:
#' Name | Unit | Meta
#' ---|---|---
#' Productivity\|Yields\|Yield by harvested area | t DM/ha | Crop yields calculated as production divided by harvested cropland area (physical area scaled by the exogenous multicropping index)
#' Productivity\|Yields\|Yield by harvested area\|+\|Crops | t DM/ha | Yield by harvested area of all crops
#' Productivity\|Yields\|Yield by harvested area\|Crops\|+\|Cereals | t DM/ha | Yield by harvested area of cereals
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
                     product_aggr = FALSE, water_aggr = watAgg)
    area <- reporthelper(x = area, dim = 3.1, level_zero_name = indicatorName,
                         detail = detail)

    if (!physical) {
      # Read in multicropping (ratio between area harvested and physical cropland area)
      multicropping <- readGDX(gdx, "f18_multicropping", "fm_multicropping",
                               format = "first_found",
                               level = "reg",
                               types = "parameters")[, getYears(area), ]
      # Correct regions
      areaREG <- area[getItems(multicropping, dim = 1.1), , ]
      # Transform crop area (physical area) into harvested area
      areaREG <- areaREG * multicropping
      # Global sum and regions
      area <- gdxAggregate(gdx, areaREG, to = level)
    }

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
