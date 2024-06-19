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
  nonag <- collapseNames(water_usage(gdx, level = "regglo", users = "sectors", sum = FALSE,
                                     seasonality = "total",
                                     digits = 10)[, , nonagsectors])
  nonagTotal <- round(dimSums(nonag, dim = 3), digits = 3)
  getNames(nonagTotal) <- "Resources|Water|Withdrawal|Non-agriculture (km3/yr)"
  out <- mbind(ag, nonagTotal)

  if (detail) {

    # report non-agricultural sectors separately
    getItems(nonag, dim = 3) <- paste0("Resources|Water|Withdrawal|Non-agriculture|",
                                       getItems(nonag, dim = 3))
    getNames(nonag) <- paste(gsub("\\.", "|", getNames(nonag)), "(km3/yr)", sep = " ")
    nonag <- summationhelper(nonag, sep = "+")

    out <- mbind(out, nonag)
  }

  return(out)
}
