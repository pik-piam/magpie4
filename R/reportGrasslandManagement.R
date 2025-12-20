#' @title reportGrasslandManagement
#' @description reports cattle related numbers
#'
#' @export
#'
#' @param gdx GDX file
#' @return Cattle values as magpie objetc
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' x <- reportGrasslandManagement(gdx)
#' }
#'
#'
#' @section Stock density variables:
#' Name | Unit | Meta
#' ---|---|---
#' Stock density\|+\|Cattle\|Managed pastures | Lsu per ha | Livestock units per hectare on managed pastures
#' Stock density\|+\|Cattle\|Rangelands | Lsu per ha | Livestock units per hectare on rangelands
#'
#' @section Total livestock unit variables:
#' Name | Unit | Meta
#' ---|---|---
#' Total lsu\|+\|Cattle\|Managed pastures | millions | Total livestock units on managed pastures
#' Total lsu\|+\|Cattle\|Rangelands | millions | Total livestock units on rangelands
#'
#' @section Land cover variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover\|Managed pastures | million ha | Area of managed pastures
#' Resources\|Land Cover\|Rangelands | million ha | Area of rangelands
#' @md

#'

reportGrasslandManagement <- function(gdx) {

  # read in data
  x <- NULL
  grass_areas <- NULL
  grass_yld <- NULL
  try({grass_areas <- readGDX(gdx, "ov31_grass_area", format = "simplest", react = "silent")[, , list("type" = "level")]})
  # try({grass_yld <- readGDX(gdx, "ov31_grass_yld", format = "simplest")[, , list("type" = "level")]})
  try({grass_yld <- grassyld(gdx)})

  if (!all(c(is.null(grass_yld), is.null(grass_areas)))) {
  grass_areas <- collapseNames(grass_areas) 
  grass_yld <- collapseNames(grass_yld) 
  total_lsus <- grass_areas * grass_yld / (8.9 * 365 / 1000) # (lsu equivalent annual consumption in tDM)
  lsu_ha <- total_lsus / grass_areas
  lsu_ha[is.nan(lsu_ha) | is.infinite(lsu_ha)] <- 0
  
  # aggregate and add global
  total_lsus_reg <- gdxAggregate(gdx, total_lsus, to = "regglo", absolute = T)
  lsu_ha_reg <- gdxAggregate(gdx, lsu_ha, to = "regglo", weight = grass_areas, absolute = F)
  grass_areas_reg <- gdxAggregate(gdx, grass_areas, to = "regglo", absolute = T)
    
  # aggreate and rename
  x <- NULL
  x <- mbind(x, setNames(lsu_ha_reg, paste0("Stock density|+|Cattle|", reportingnames(getNames(lsu_ha_reg, dim = 1)), " (Lsu per ha)")))
  x <- mbind(x, setNames(total_lsus_reg, paste0("Total lsu|+|Cattle|", reportingnames(getNames(total_lsus_reg, dim = 1)), " (millions)")))
  x <- mbind(x, setNames(grass_areas_reg, paste0("Resources|Land Cover|", reportingnames(getNames(grass_areas_reg, dim = 1)), " (million ha)")))
  return(x)
  } else {
    x <- "Disabled (no managed pastures)"
  }  
}
