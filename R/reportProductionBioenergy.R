#' @title reportProductionBioenergy
#' @description reports 2nd gen bioenergy production
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return production as MAgPIE object. Unit: see names
#' @author Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportProductionBioenergy(gdx)
#'   }
#'
#' @section Bioenergy production variables:
#' Name | Unit | Meta
#' ---|---|---
#' Production\|Bioenergy\|2nd generation | EJ/yr | Second generation bioenergy production (grassy and woody crops)
#' Production\|Bioenergy\|2nd generation\|++\|Grassy bioenergy crops | EJ/yr | Production from short rotation grasses
#' Production\|Bioenergy\|2nd generation\|++\|Woody bioenergy crops | EJ/yr | Production from short rotation trees
#' Production\|Bioenergy\|2nd generation\|Cumulative | EJ | Cumulative second generation bioenergy production
#' @md


reportProductionBioenergy <- function(gdx, detail = FALSE, level = "regglo") {

  #annual
  x <- collapseNames(
    production(
      gdx = gdx,
      level = level,
      products = c("begr", "betr"),
      attributes = "ge",
      product_aggr = FALSE,
      water_aggr = TRUE,
      cumulative = FALSE
    ),
    collapsedim = "attributes"
  ) / 1000

  out <- reporthelper(x = x, dim = 3.1, level_zero_name = "Production|Bioenergy|2nd generation", detail = detail)
  out <- summationhelper(out, sep = "++")
  getNames(out) <- paste(getNames(out), "(EJ/yr)", sep = " ")
  annual <- out

  #cumulative
  x <- collapseNames(
    production(
      gdx = gdx,
      level = level,
      products = c("begr", "betr"),
      attributes = "ge",
      product_aggr = FALSE,
      water_aggr = TRUE,
      cumulative = TRUE
    ),
    collapsedim = "attributes"
  ) / 1000

  out <- reporthelper(x = x, dim = 3.1,
                      level_zero_name = "Production|Bioenergy|2nd generation|Cumulative", detail = detail)
  out <- summationhelper(out, sep = "++")
  getNames(out) <- paste(getNames(out), "(EJ)", sep = " ")
  cumulative <- out

  #combine
  out <- mbind(annual, cumulative)

  return(out)
}