#' changelogVariables
#'
#' Returns a named vector of variables to put into the data changelog, see \code{\link{addToDataChangelog}}.
#'
#' @author Pascal Sauer
#' @export
changelogVariables <- function() {
  return(c(lucEmisRaw = "Emissions|CO2|Land RAW|+|Land-use Change",
           tau = "Productivity|Landuse Intensity Indicator Tau",
           cropland = "Resources|Land Cover|+|Cropland",
           irrigated = "Resources|Land Cover|Cropland|Area actually irrigated",
           pasture = "Resources|Land Cover|+|Pastures and Rangelands",
           forest = "Resources|Land Cover|+|Forest",
           other = "Resources|Land Cover|+|Other Land",
           production = "Production",
           costs = "Costs",
           foodExp = "Household Expenditure|Food|Expenditure"))
}
