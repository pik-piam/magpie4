#' @title reportWaterUsage
#' @description reports water usage for agricultural sector, crops and livestock
#'              and non-agricultural sector
#'
#' @param gdx    GDX file
#' @param detail logical. Setting to FALSE reports for agricultural sector,
#'               TRUE reports for combined, crops and livestock separately
#' @return water usage as MAgPIE object Unit: see names
#' @author Florian Humpenoeder, Vartika Singh, Miodrag Stevanovic, Felicitas Beier
#'
#' @importFrom magpiesets reporthelper
#' @export
#'
#' @examples
#' \dontrun{
#' x <- reportWaterUsage(gdx)
#' }
#'
#' @section Agricultural water withdrawal variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Water\|Withdrawal\|Agriculture | km3/yr | Total agricultural water withdrawal
#' Resources\|Water\|Withdrawal\|Agriculture\|Crops | km3/yr | Water withdrawal for crop irrigation
#' Resources\|Water\|Withdrawal\|Agriculture\|Crops\|+\|Crops | km3/yr | Water withdrawal for food and feed crops
#' Resources\|Water\|Withdrawal\|Agriculture\|Crops\|+\|Bioenergy crops | km3/yr | Water withdrawal for bioenergy crops
#' Resources\|Water\|Withdrawal\|Agriculture\|Livestock\|+\|Livestock products | km3/yr | Water withdrawal for livestock
#'
#' @section Agricultural water consumption variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Water\|Consumption\|Agriculture | km3/yr | Total agricultural water consumption
#' Resources\|Water\|Consumption\|Agriculture\|Crops | km3/yr | Water consumption for crop irrigation
#' Resources\|Water\|Consumption\|Agriculture\|Livestock | km3/yr | Water consumption for livestock
#'
#' @section Non-agricultural water variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Water\|Withdrawal\|Non-agriculture | km3/yr | Total non-agricultural water withdrawal
#' Resources\|Water\|Withdrawal\|Non-agriculture\|+\|domestic | km3/yr | Water withdrawal for domestic use
#' Resources\|Water\|Withdrawal\|Non-agriculture\|+\|manufacturing | km3/yr | Water withdrawal for manufacturing sector
#' Resources\|Water\|Withdrawal\|Non-agriculture\|+\|electricity | km3/yr | Water withdrawal for electricity generation
#' Resources\|Water\|Consumption\|Non-agriculture | km3/yr | Total non-agricultural water consumption
#' @md

#'
reportWaterUsage <- function(gdx, detail = TRUE, level = "regglo") {

  .reportHelper <- function(name, data, unit = "(km3/yr)") {
    result <- reporthelper(x = data, dim = 3.1, detail = detail,
                           level_zero_name = name)
    getNames(result) <- paste(gsub("\\.", "|", getNames(result)), unit, sep = " ")
    return(summationhelper(result, sep = "+"))
  }

  # Agricultural water usage
  # withdrawal
  ag           <- water_usage(gdx, level = level, users = "sectors", abstractiontype = "withdrawal",
                              sum = FALSE, digits = 3)[, , "agriculture"]
  getNames(ag) <- "Resources|Water|Withdrawal|Agriculture (km3/yr)"

  # consumption
  consumption           <- water_usage(gdx, level = level, users = "sectors", abstractiontype = "consumption",
                                       sum = FALSE, digits = 3)[, , "agriculture"]
  getNames(consumption) <- "Resources|Water|Consumption|Agriculture (km3/yr)"

  # Irrigation
  # irrigation water withdrawal
  irrigWithdrawal <- .reportHelper("Resources|Water|Withdrawal|Agriculture|Crops",
                                   water_usage(gdx, level = level, users = "kcr", abstractiontype = "withdrawal",
                                               sum = FALSE, digits = 3))

  # irrigation water consumption
  irrigConsumption <- .reportHelper("Resources|Water|Consumption|Agriculture|Crops",
                                    water_usage(gdx, level = level, users = "kcr", abstractiontype = "consumption",
                                                sum = FALSE, digits = 3))

  # Livestock
  # livestock withdrawal
  livestockWithdrawal <- .reportHelper("Resources|Water|Withdrawal|Agriculture|Livestock",
                                       water_usage(gdx, abstractiontype = "withdrawal",
                                                   level = level, users = "kli", sum = FALSE, digits = 3))

  # livestock consumption
  livestockConsumption <- .reportHelper("Resources|Water|Consumption|Agriculture|Livestock",
                                        water_usage(gdx, abstractiontype = "consumption",
                                                    level = level, users = "kli", sum = FALSE, digits = 3))

  # Non-agricultural water usage (in entire year)
  nonagsectors <- c("domestic", "manufacturing", "electricity")
  # withdrawal
  nonag_ww <- collapseNames(water_usage(gdx, level = level, users = "sectors", sum = FALSE,
                                        seasonality = "total", abstractiontype = "withdrawal",
                                        digits = 10)[, , nonagsectors])
  nonagTotal_ww <- round(dimSums(nonag_ww, dim = 3.1), digits = 3)
  getNames(nonagTotal_ww) <- "Resources|Water|Withdrawal|Non-agriculture (km3/yr)"

  # consumption
  nonag_wc <- collapseNames(water_usage(gdx, level = level, users = "sectors", sum = FALSE,
                                        seasonality = "total", abstractiontype = "consumption",
                                        digits = 10)[, , nonagsectors])
  nonagTotal_wc <- round(dimSums(nonag_wc, dim = 3.1), digits = 3)
  getNames(nonagTotal_wc) <- "Resources|Water|Consumption|Non-agriculture (km3/yr)"

  # report non-agricultural sectors separately
  # withdrawal
  getItems(nonag_ww, dim = 3) <- paste0("Resources|Water|Withdrawal|Non-agriculture|",
                                        getItems(nonag_ww, dim = 3))
  getNames(nonag_ww) <- paste(gsub("\\.", "|", getNames(nonag_ww)), "(km3/yr)", sep = " ")
  nonag_ww <- summationhelper(nonag_ww, sep = "+")

  # consumption
  getItems(nonag_wc, dim = 3) <- paste0("Resources|Water|Consumption|Non-agriculture|",
                                        getItems(nonag_wc, dim = 3))
  getNames(nonag_wc) <- paste(gsub("\\.", "|", getNames(nonag_wc)), "(km3/yr)", sep = " ")
  nonag_wc <- summationhelper(nonag_wc, sep = "+")

  out <- mbind(
    ag, consumption,
    irrigWithdrawal, irrigConsumption,
    livestockWithdrawal, livestockConsumption,
    nonagTotal_wc, nonagTotal_ww,
    nonag_ww, nonag_wc
  )

  return(out)
}
