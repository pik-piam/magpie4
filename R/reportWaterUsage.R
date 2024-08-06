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
reportWaterUsage <- function(gdx, detail = TRUE) {

  # Agricultural water usage
  ag           <- water_usage(gdx, level = "regglo", users = "sectors",
                              sum = FALSE, digits = 3)[, , "agriculture"]
  getNames(ag) <- "Resources|Water|Withdrawal|Agriculture (km3/yr)"

  if (detail) {

      y   <- water_usage(gdx, level = "regglo", users = "kcr", sum = FALSE, digits = 3)
      tmp <- reporthelper(x = y, dim = 3.1, detail = detail,
                         level_zero_name = "Resources|Water|Withdrawal|Agriculture|Crops")
      getNames(tmp) <- paste(gsub("\\.", "|", getNames(tmp)), "(km3/yr)", sep = " ")
      tmp <- summationhelper(tmp, sep = "+")

      ag <- mbind(ag, tmp)

      z   <- water_usage(gdx, level = "regglo", users = "kli", sum = FALSE, digits = 3)
      tmp <- reporthelper(x = z, dim = 3.1, detail = detail,
                         level_zero_name = "Resources|Water|Withdrawal|Agriculture|Livestock")
      getNames(tmp) <- paste(gsub("\\.", "|", getNames(tmp)), "(km3/yr)", sep = " ")
      tmp <- summationhelper(tmp, sep = "+")

      ag <- mbind(ag, tmp)
  }

  # Non-agricultural water usage (in entire year)
  nonagsectors <- c("domestic", "manufacturing", "electricity")
  # withdrawal
  nonag_ww <- collapseNames(water_usage(gdx, level = "regglo", users = "sectors", sum = FALSE,
                                     seasonality = "total", abstractiontype = "withdrawal",
                                     digits = 10)[, , nonagsectors])
  nonagTotal_ww <- round(dimSums(nonag_ww, dim = 3.1), digits = 3)
  getNames(nonagTotal_ww) <- "Resources|Water|Withdrawal|Non-agriculture (km3/yr)"
  out <- mbind(ag, nonagTotal_ww)

  nonag_wc <- collapseNames(water_usage(gdx, level = "regglo", users = "sectors", sum = FALSE,
                                     seasonality = "total", abstractiontype = "consumption",
                                     digits = 10)[, , nonagsectors])
  # consumption
  nonag_wc <- collapseNames(water_usage(gdx, level = "regglo", users = "sectors", sum = FALSE,
                                        seasonality = "total", abstractiontype = "consumption",
                                        digits = 10)[, , nonagsectors])
  nonagTotal_wc <- round(dimSums(nonag_wc, dim = 3.1), digits = 3)
  getNames(nonagTotal_wc) <- "Resources|Water|Consumption|Non-agriculture (km3/yr)"
  out <- mbind(out, nonagTotal_wc)

  if (detail) {

    # report non-agricultural sectors separately
    # withdrawal
    getItems(nonag_ww, dim = 3) <- paste0("Resources|Water|Withdrawal|Non-agriculture|",
                                       getItems(nonag_ww, dim = 3))
    getNames(nonag_ww) <- paste(gsub("\\.", "|", getNames(nonag_ww)), "(km3/yr)", sep = " ")
    nonag_ww <- summationhelper(nonag_ww, sep = "+")
    out <- mbind(out, nonag_ww)

    # consumption
    getItems(nonag_wc, dim = 3) <- paste0("Resources|Water|Consumption|Non-agriculture|",
                                          getItems(nonag_wc, dim = 3))
    getNames(nonag_wc) <- paste(gsub("\\.", "|", getNames(nonag_wc)), "(km3/yr)", sep = " ")
    nonag_wc <- summationhelper(nonag_wc, sep = "+")
    out <- mbind(out, nonag_wc)
  }

  return(out)
}
