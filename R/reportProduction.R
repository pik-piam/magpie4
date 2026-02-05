#' @title reportProduction
#' @description reports production
#'
#' @import magpiesets
#' @export
#'
#' @param gdx    GDX file
#' @param level aggregation level of retruned data ("regglo" by default)
#' @param detail if detail=FALSE, the subcategories of groups are not reported
#'               (e.g. "soybean" within "oilcrops")
#' @param agmip  if agmip = TRUE, additional sector aggregates
#'               required for AgMIP are reported (e.g. "AGR")
#' @return production as MAgPIE object. Unit: see names
#' @author Benjamin Leon Bodirsky, Isabelle Weindl
#' @examples
#'
#'   \dontrun{
#'     x <- reportProduction(gdx)
#'   }
#'
#'
#' @section Production variables:
#' Name | Unit | Meta
#' ---|---|---
#' Production | Mt DM/yr | Total agricultural production
#' Production\|+\|Crops | Mt DM/yr | Production of crops
#' Production\|+\|Livestock products | Mt DM/yr | Production of livestock products (excluding fish)
#' Production\|+\|Secondary products | Mt DM/yr | Production of secondary products (processed agricultural goods)
#' Production\|+\|Pasture | Mt DM/yr | Production of pasture biomass
#' Production\|+\|Bioenergy crops | Mt DM/yr | Production of second-generation bioenergy crops (short rotation grasses, short rotation trees)
#' @md
reportProduction <- function(gdx, detail = FALSE, agmip = FALSE, level = "regglo") {

  x   <- production(gdx = gdx, level = level, products = readGDX(gdx, "kall"),
                    product_aggr = FALSE, water_aggr = TRUE)
  out <- reporthelper(x = x, dim = 3.1, level_zero_name = "Production", detail = detail)
  out <- summationhelper(out)

  if (agmip) {
    agGroup <- c("Production|+|Crops", "Production|+|Livestock products", "Production|+|Pasture")
    agr     <- dimSums(out[, , agGroup], dim = 3.1)
    getNames(agr) <- "Production|Primary agricultural products"
    out <- mbind(out, agr)
  }

  getNames(out)[1] <- "Production"
  getNames(out)    <- paste(getNames(out), "(Mt DM/yr)", sep = " ")
  return(out)
}
