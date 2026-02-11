#' @title ValueMaterialDemand
#' @description calculates agricultural costs without taxes and incentives (i.e. GHG taxes and BII incentives)
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, reg, glo or regglo
#' @author David M Chen

#' @examples
#' \dontrun{
#' x <- ValueMaterialDemand(gdx)
#' }
#'
ValueMaterialDemand <- function(gdx, file = NULL, level = "regglo") {

  dem <- demand(gdx = gdx, level = level, products = "kall")
  dem <- dem[, , c("other_util", "bioenergy")]

  #constant/standard prices price initialization - FAOSTAT
  prices <- readGDX(gdx, "f15_prices_initial")
  citems <- intersect(getNames(dem, dim = 2), getNames(prices))
  cyears <- intersect(getYears(dem), getYears(prices))

  value <- dem[, cyears, citems] * prices[, cyears, citems]

  return(value)
}
