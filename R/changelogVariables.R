#' changelogVariables
#'
#' Returns a named vector of variables to put into the data changelog, see \code{\link{addToDataChangelog}}.
#'
#' @author Pascal Sauer
#' @export
changelogVariables <- function() {
  return(c(lucEmisRaw = "Emissions|CO2|Land RAW|+|Land-use Change (Mt CO2/yr)",
           tau = "Productivity|Landuse Intensity Indicator Tau (Index)",
           cropland = "Resources|Land Cover|+|Cropland (million ha)",
           irrigated = "Resources|Land Cover|Cropland|Area actually irrigated (million ha)",
           pasture = "Resources|Land Cover|+|Pastures and Rangelands (million ha)",
           forest = "Resources|Land Cover|+|Forest (million ha)",
           other = "Resources|Land Cover|+|Other Land (million ha)",
           production = "Production (Mt DM/yr)",
           costs = "Costs (million US$2017/yr)",
           foodExp = "Household Expenditure|Food|Expenditure (US$2017/capita)"))
}
