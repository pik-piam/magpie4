#' @title reportNitrogenPollution
#' @description Reports total Nitrogen Pollution as the sum of surplus from cropland, pasture, awms, consumption and non-agricutlural land
#'
#' @importFrom magpiesets reportingnames
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @author Benjamin Leon Bodirsky, Michael Crawford
#' @seealso
#' \code{\link{NitrogenBudget}}
#'
#' @examples
#' \dontrun{
#' x <- reportNitrogenPollution(gdx)
#' }
#'
#' @section Nitrogen pollution variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Nitrogen\|Pollution\|Surplus | Mt Nr/yr | Total nitrogen pollution surplus from all sources
#' Resources\|Nitrogen\|Pollution\|Surplus\|+\|Cropland | Mt Nr/yr | Nitrogen surplus from cropland
#' Resources\|Nitrogen\|Pollution\|Surplus\|+\|Pasture | Mt Nr/yr | Nitrogen surplus from pasture
#' Resources\|Nitrogen\|Pollution\|Surplus\|+\|Animal waste management | Mt Nr/yr | Nitrogen losses from animal waste management systems
#' Resources\|Nitrogen\|Pollution\|Surplus\|+\|Non-agricultural land | Mt Nr/yr | Nitrogen surplus from non-agricultural land
#' Resources\|Nitrogen\|Pollution\|Surplus\|+\|End-of-life losses | Mt Nr/yr | Nitrogen losses from food consumption and waste
#'
#' @section Nitrogen aggregate variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Nitrogen\|Nutrient surplus from agricultural land | Mt Nr/yr | Nitrogen surplus from cropland and pasture
#' Resources\|Nitrogen\|Nutrient surplus from agricultural land and manure management | Mt Nr/yr | Nitrogen surplus from cropland, pasture, and AWMS
#' Resources\|Nitrogen\|Nutrient surplus from all land and manure management | Mt Nr/yr | Nitrogen surplus from all land and manure management
#' @md

#'
reportNitrogenPollution <- function(gdx, level = "regglo") {

  # Cropland surplus
  cropland <- NitrogenBudget(gdx, level = "reg")[, , "surplus"]

  # Pasture surplus
  pasture <- NitrogenBudgetPasture(gdx, level = "reg")[, , "surplus"]

  # Animal Waste Management Surplus (AWMS)
  confinement <- dimSums(readGDX(gdx, "ov_manure_confinement")[, , "level"][, , "nr"], dim = 3)
  recycling   <- dimSums(readGDX(gdx, "ov_manure_recycling")[, , "level"][, , "nr"], dim = 3)
  awms        <- confinement - recycling

  # Non-agricultural land surplus
  nb_nonagland <- NitrogenBudgetNonagland(gdx, level = "reg")[, , "surplus"]
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
  # aggregate to requested aggregation level
  if (level %in% c("reg", "glo", "regglo") || isCustomAggregation(level)) {
    combined <- gdxAggregate(gdx, combined, to = level)
  } else {
    stop("reportNitrogenPollution does not support aggregation level: ", level)
  }

  return(combined)
}
