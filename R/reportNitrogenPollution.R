#' @title reportNitrogenPollution
#' @description Reports total Nitrogen Pollution as the sum of surplus from cropland, pasture, awms, consumption and non-agricutlural land
#'
#' @importFrom magpiesets reportingnames
#' @export
#'
#' @param gdx GDX file
#' @param dir magpie output directory that contains gridded Nitrogen Data
#' @author Benjamin Leon Bodirsky, Michael Crawford
#' @seealso
#' \code{\link{NitrogenBudget}}
#'
#' @examples
#' \dontrun{
#' x <- reportNitrogenPollution(gdx)
#' }
#'
reportNitrogenPollution <- function(gdx, dir = ".") {

  # Cropland surplus
  cropland <- NitrogenBudget(gdx, level = "reg", dir = dir)[, , "surplus"]

  # Pasture surplus
  pasture <- NitrogenBudgetPasture(gdx, level = "reg", dir = dir)[, , "surplus"]

  # Animal Waste Management Surplus (AWMS)
  confinement <- dimSums(readGDX(gdx, "ov_manure_confinement")[, , "level"][, , "nr"], dim = 3)
  recycling   <- dimSums(readGDX(gdx, "ov_manure_recycling")[, , "level"][, , "nr"], dim = 3)
  awms        <- confinement - recycling

  # Non-agricultural land surplus
  nb_nonagland <- NitrogenBudgetNonagland(gdx, level = "reg", dir = dir)[, , "surplus"]
  nonagland    <- dimSums(nb_nonagland, dim = 3)

  # Consumption losses
  raw_demand  <- demand(gdx, attributes = "nr", product_aggr = TRUE)
  consumption <- dimSums(raw_demand[, , c("food", "other_util", "bioenergy", "waste")], dim = 3)

  # combine individual streams
  combined <- mbind(
    setNames(cropland,    "Resources|Nitrogen|Pollution|Surplus|+|Cropland (Mt Nr/yr)"),
    setNames(pasture,     "Resources|Nitrogen|Pollution|Surplus|+|Pasture (Mt Nr/yr)"),
    setNames(awms,        "Resources|Nitrogen|Pollution|Surplus|+|Animal waste management (Mt Nr/yr)"),
    setNames(nonagland,   "Resources|Nitrogen|Pollution|Surplus|+|Non-agricultural land (Mt Nr/yr)"),
    setNames(consumption, "Resources|Nitrogen|Pollution|Surplus|+|End-of-life losses (Mt Nr/yr)")
  )

  # total of all five streams
  combined <- mbind(
    setNames(dimSums(combined, dim = 3), "Resources|Nitrogen|Pollution|Surplus (Mt Nr/yr)"),
    combined
  )

  # -----------------------------------------------------------------------------------------------------------------
  # agricultural-only surplus (cropland + pasture + awms)
  surplus_agriAWMS <- dimSums(mbind(cropland, pasture, awms), dim = 3)
  combined <- mbind(
    setNames(
      surplus_agriAWMS,
      "Resources|Nitrogen|Nutrient surplus from agricultural land and manure management (Mt Nr/yr)"
    ),
    combined
  )

  # all-land surplus (cropland + pasture)
  surplus_agri <- dimSums(mbind(cropland, pasture), dim = 3)
  combined <- mbind(
    setNames(
      surplus_agri,
      "Resources|Nitrogen|Nutrient surplus from agricultural land (Mt Nr/yr)"
    ),
    combined
  )

  # all-land surplus (cropland + pasture + awms + nonagland)
  surplus_all <- dimSums(mbind(cropland, pasture, awms, nonagland), dim = 3)
  combined <- mbind(
    setNames(
      surplus_all,
      "Resources|Nitrogen|Nutrient surplus from all land and manure management (Mt Nr/yr)"
    ),
    combined
  )

  # -----------------------------------------------------------------------------------------------------------------
  # add global total
  combined <- mbind(
    combined,
    setCells(dimSums(combined, dim = 1), "World")
  )

  return(combined)
}
