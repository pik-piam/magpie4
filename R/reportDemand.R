#' @title reportDemand
#' @description reports Demand for Food, Feed, Processing, Material, Bioenergy, Seed and Supply Chain Loss
#'
#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @param agmip if agmip=T, additional sector aggregates required for agmip are reported (e.g. "AGR")
#' @param level The level at which the report data should be aggregated.
#' @return demand as MAgPIE object (Mt DM)
#' @author Benjamin Leon Bodirsky, Isabelle Weindl
#' @examples
#'
#'   \dontrun{
#'     x <- reportDemand()
#'   }
#'
#'
#' @section Demand variables:
#' Name | Unit | Meta
#' ---|---|---
#' Demand | Mt DM/yr | Total demand for agricultural products including food, feed, processing, material, bioenergy, seed and supply chain loss
#' Demand\|++\|Crops | Mt DM/yr | Demand for crops including food, feed products and bioenergy (1st and 2nd generation crops)
#' Demand\|++\|Livestock products | Mt DM/yr | Demand for livestock products (excluding fish)
#' Demand\|++\|Secondary products | Mt DM/yr | Demand for secondary products (processed agricultural goods)
#' Demand\|++\|Pasture | Mt DM/yr | Demand for pasture biomass
#' Demand\|++\|Bioenergy crops | Mt DM/yr | Demand for second-generation bioenergy crops
#'
#' @section Demand by use variables:
#' Name | Unit | Meta
#' ---|---|---
#' Demand\|Food | Mt DM/yr | Demand for food consumption
#' Demand\|Feed | Mt DM/yr | Demand for animal feed
#' Demand\|Processing | Mt DM/yr | Demand for food processing
#' Demand\|Material | Mt DM/yr | Demand for material use (non-food, non-feed)
#' Demand\|Bioenergy | Mt DM/yr | Demand for bioenergy production
#' Demand\|Seed | Mt DM/yr | Demand for seeds
#' Demand\|Supply Chain Loss | Mt DM/yr | Losses in the supply chain
#' @md

reportDemand <- function(gdx, detail = FALSE, agmip = FALSE, level = "regglo") {
  out <- NULL
  x <- demand(gdx, level = level)
  getNames(x, dim = 1) <- reportingnames(getNames(x, dim = 1))

  for (type in getNames(x, dim = 1)) {
    tmp <- collapseNames(x[, , type], collapsedim = 1)
    # demand.R renamed dim=3.1
    tmp <- reporthelper(x = tmp, level_zero_name = paste0("Demand|", type), detail = detail, dim = 3.1)
    getNames(tmp) <- paste(getNames(tmp), "(Mt DM/yr)", sep = " ")
    out <- mbind(out, tmp)
  }
  out <- summationhelper(out)

  # Sum over all demands
  sum <- dimSums(x[, , ], dim = 3.1)
  # demand.R renamed dim=3.1
  sum <- reporthelper(x = sum, level_zero_name = "Demand", detail = detail, dim = 3.1)
  sum <- summationhelper(sum)
  if (agmip == TRUE) {
    agr_group <- c(
      "Demand|+|Crops",
      "Demand|+|Livestock products",
      "Demand|+|Pasture"
    ) #"Demand|+|Forage"
    agr <- dimSums(sum[, , agr_group], dim = 3.1)
    getNames(agr) <- "Demand|Primary agricultural products"
    sum <- mbind(sum, agr)
  }
  getNames(sum)[1] <- "Demand"
  getNames(sum) <- paste(getNames(sum), "(Mt DM/yr)", sep = " ")
  getNames(sum) <- gsub(pattern = "Demand\\|\\+", replacement = "Demand|++", x = getNames(sum))

  out <- mbind(out, sum)

  return(out)
}
