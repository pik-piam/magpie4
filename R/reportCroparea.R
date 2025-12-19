#' @title reportCroparea
#' @description reports croparea
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return Croparea as MAgPIE object (million Ha/yr)
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportCroparea(gdx)
#' }
#'
#' @section Croparea variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover\|Cropland\|Croparea | million ha | Total physical cropland area used for crop production
#' Resources\|Land Cover\|Cropland\|Croparea\|+\|Crops | million ha | Cropland area for food and feed crops
#' Resources\|Land Cover\|Cropland\|Croparea\|Crops\|+\|Cereals | million ha | Cropland for cereals (maize, rice, temperate cereals and tropical cereals)
#' Resources\|Land Cover\|Cropland\|Croparea\|Crops\|+\|Oil crops | million ha | Cropland for oil crops (cotton seed, groundnuts, oilpalms, other oil crops, soybean, sunflower)
#' Resources\|Land Cover\|Cropland\|Croparea\|Crops\|+\|Sugar crops | million ha | Cropland for sugar crops (sugar beet, sugar cane)
#' Resources\|Land Cover\|Cropland\|Croparea\|Crops\|+\|Other crops | million ha | Cropland for other crops (fruits, vegetables, nuts, potatoes, pulses, tropical roots)
#' Resources\|Land Cover\|Cropland\|Croparea\|+\|Bioenergy crops | million ha | Cropland for second-generation bioenergy crops (short rotation grasses, short rotation trees)
#' Resources\|Land Cover\|Cropland\|Croparea\|++\|Irrigated | million ha | Irrigated cropland (physical area)
#' Resources\|Land Cover\|Cropland\|Croparea\|++\|Rainfed | million ha | Rainfed cropland (physical area)
#' @md

#'
reportCroparea <- function(gdx, detail = FALSE) {

  x <- NULL

  out <- croparea(gdx, level = "regglo", products = "kcr",
                  product_aggr = FALSE, water_aggr = TRUE)
  # Adapting reportingnames for inclusing of fallow land and tree cover in reportLandUse
  ReportingnameCroparea <- "Resources|Land Cover|Cropland|Croparea"

  out <- reporthelper(x = out, dim = 3.1,
                      level_zero_name = ReportingnameCroparea, detail = detail)
  getNames(out) <- paste(gsub("\\.", "|", getNames(out)), "(million ha)", sep = " ")
  out <- summationhelper(out, sep = "+")


  out2 <- croparea(gdx, level = "regglo", products = "kcr",
                  product_aggr = FALSE, water_aggr = FALSE)
  total <- dimSums(out2, dim = 3.1)
  getNames(total) <- paste(ReportingnameCroparea, "|", reportingnames(getNames(total)),
                           " (million ha)", sep = "")
  out2 <- reporthelper(x = out2, dim = 3.1,
                      level_zero_name = ReportingnameCroparea, detail = detail)
  getNames(out2) <- paste(gsub("\\.", "|", getNames(out2)), "(million ha)", sep = " ")
  out2 <- mbind(out2, total)
  out2 <- summationhelper(out2, sep = "++")

  x <- mbind(out, out2)

  return(x)
}
