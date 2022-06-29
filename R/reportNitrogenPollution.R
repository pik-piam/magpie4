#' @title reportNitrogenPollution
#' @description Reports total Nitrogen Pollution as the sum of surplus from cropland, pasture, awms, consumption and non-agricutlural land
#'
#' @importFrom magpiesets reportingnames
#' @export
#'
#' @param gdx GDX file
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{NitrogenBudget}}
#'
#' @examples
#' \dontrun{
#' x <- reportNitrogenPollution(gdx)
#' }
#'
reportNitrogenPollution <- function(gdx) {

  cropland <- NitrogenBudget(gdx, level = "reg")[,,"surplus"]
  pasture <- NitrogenBudgetPasture(gdx, level = "reg")[,,"surplus"]
  awms <- dimSums(readGDX(gdx,"ov_manure_confinement")[,,"level"][,,"nr"],dim=3)-dimSums(readGDX(gdx,"ov_manure_recycling")[,,"level"][,,"nr"],dim=3)
  consumption <- dimSums(demand(gdx,attributes="nr",product_aggr=TRUE)[,,c("food","other_util","bioenergy","waste")],dim=3)
  nonagland <- dimSums(NitrogenBudgetNonagland(gdx,level="reg")[,,"surplus"],dim=3)
  combined = mbind(
    setNames(cropland, "Resources|Nitrogen|Pollution|Surplus|+|Cropland (Mt Nr/yr)"),
    setNames(pasture, "Resources|Nitrogen|Pollution|Surplus|+|Pasture (Mt Nr/yr)"),
    setNames(awms, "Resources|Nitrogen|Pollution|Surplus|+|Animal Waste Management (Mt Nr/yr)"),
    setNames(nonagland, "Resources|Nitrogen|Pollution|Surplus|+|Non-agricultural land (Mt Nr/yr)"),
    setNames(consumption, "Resources|Nitrogen|Pollution|Surplus|+|End of life losses (Mt Nr/yr)")
  )
  combined = mbind(
    setNames(dimSums(combined,dim=3),"Resources|Nitrogen|Pollution|Surplus (Mt Nr/yr)"),
    combined
  )
  combined = mbind(
    combined,
    setCells(dimSums(combined,dim=1),"GLO")
  )
  return(combined)
}
