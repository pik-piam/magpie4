#' @title reportYields
#' @description reports yields
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx      GDX file
#' @param detail   if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param physical if true (default) physical area (croparea) used for yield calculation;
#'                 if false harvested area used for yield calculation
#' @return yield as MAgPIE object (Mt DM/ha)
#' @importFrom magpiesets reporthelper
#' @author Florian Humpenoeder, Xiaoxi Wang, Kristine karstens, Abhijeet Mishra, Felicitas Beier
#' @examples
#' \dontrun{
#' x <- reportYields(gdx)
#' }
#'
reportYields <- function(gdx, detail = FALSE, physical = TRUE) {

  if (sum(fallow(gdx) > 0)) {
    indicatorName <- "Productivity|Yield by physical area"
  } else {
    indicatorName <- "Productivity|Yield"
  }

  yieldWaterAgg <- function(watAgg = TRUE, sumSep = "+") {

    prod <- production(gdx, level = "regglo", products = readGDX(gdx, "kcr"),
                       product_aggr = FALSE, water_aggr = watAgg)
    prod <- reporthelper(x = prod, dim = 3.1, level_zero_name = indicatorName,
                         detail = detail)

    area <- croparea(gdx, level = "regglo", products = readGDX(gdx, "kcr"),
                     product_aggr = FALSE, water_aggr = watAgg)
    area <- reporthelper(x = area, dim = 3.1, level_zero_name = indicatorName,
                         detail = detail)

    if (!physical) {
      # Read in multicropping (ratio between area harvested and physical cropland area)
      multicropping <- readGDX(gdx, "f18_multicropping", "fm_multicropping",
                               format = "first_found",
                               level = "regglo",
                               types = "parameters")[, getYears(area), ]
      # Correct regions
      areaREG <- area[getItems(multicropping, dim = 1.1), , ]
      # Transform crop area (physical area) into harvested area
      areaREG <- areaREG * multicropping
      # Global sum and regions
      area[, , ] <- NA
      area[getItems(multicropping, dim = 1.1), , ] <- areaREG
      area["GLO", , ] <- dimSums(areaREG, dim = 1)
    }

    out <- ifelse(prod > 1e-10, prod / area, NA)
    getNames(out) <- paste(gsub("\\.", "|", getNames(out)), "(t DM/ha)", sep = " ")

    if (length(sumSep) != 0) {
      out <- summationhelper(out, sep = sumSep)
    }
    return(out)
  }

  x <- mbind(yieldWaterAgg(watAgg = TRUE, sumSep = NULL),
             yieldWaterAgg(watAgg = FALSE, sumSep = NULL))

  pasture <- yields(gdx, level = "regglo", products = "pasture", attributes = "dm")
  pasture <- summationhelper(reporthelper(x = pasture, dim = 3.1,
                                          level_zero_name = indicatorName, detail = detail),
                             sep = NULL)
  getNames(pasture) <- paste(getNames(pasture), "(t DM/ha)", sep = " ")

  x <- mbind(x, pasture)

  return(x)
}
