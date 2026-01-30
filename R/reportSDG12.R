#' @title reportSDG12
#' @description reports all SDG indicators relevant for SD12 - Sustainable Production and Consumption
#' @import magpiesets
#'
#' @export
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportSDG12(gdx)
#'   }
#'
#'
#' @section SDG12 Sustainable consumption variables:
#' Name | Unit | Meta
#' ---|---|---
#' SDG\|SDG12\|Material footprint | tDM/capita/yr | Per-capita crop demand (material footprint proxy)
#' SDG\|SDG12\|Food waste | kcal/cap/day | Per-capita daily food waste (caloric availability minus intake)
#' SDG\|SDG12\|Food waste total | Mt DM/yr | Total food waste in dry matter
#' SDG\|SDG12\|Food loss | Mt DM/yr | Food losses in supply chain (pre-consumer waste)
#' @md


reportSDG12 <- function(gdx, level = "regglo") {
  x <- NULL

  population <- population(gdx, level = level)

  # SDG|SDG12|Material footprint
  # better backcalculation of footprint would be nice! E.g impacts by ton, accounting for average trade patterns
  x <- mbind(x, sdgIndicator("SDG|SDG12|Material footprint", "tDM/capita/yr",
                             dimSums(demand(gdx, level = level)[, , findset("kcr")]) / population))

  # SDG|SDG12|Food waste
  x <- mbind(x, sdgIndicator("SDG|SDG12|Food waste", "kcal/cap/day",
                             Kcal(gdx, level = level) - IntakeDetailed(gdx, level = level, product_aggr = TRUE)))

  # SDG|SDG12|Food waste total
  att <- collapseNames(readGDX(gdx = gdx,
                               "fm_nutrition_attributes",
                               "f15_nutrition_attributes",
                               format = "first_found")[, , "kcal"]) * 1000000 # kcal per tDM

  kcal <- Kcal(gdx, level = level, product_aggr = FALSE) * population * 365 # mio. kcal
  intakeDetailed <- IntakeDetailed(gdx, level = level, product_aggr = FALSE) * population * 365 # mio. kcal
  
  kcal <- dimSums(kcal / att[, getYears(kcal), getNames(kcal, dim = 1)], dim = 3)
  intakeDetailed <- dimSums(intakeDetailed / att[, getYears(intakeDetailed), getNames(intakeDetailed, dim = 1)], dim = 3)
  out <- kcal - intakeDetailed
  x <- mbind(x, sdgIndicator("SDG|SDG12|Food waste total", "Mt DM/yr", out))

  # SDG|SDG12|Food loss
  out <- dimSums(demand(gdx, level = level)[, , readGDX(gdx, "kall")][, , "waste"])
  x <- mbind(x, sdgIndicator("SDG|SDG12|Food loss", "Mt DM/yr", out))

  return(x)
}
