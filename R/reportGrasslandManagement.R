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

reportGrasslandManagement <- function(gdx) {

  # read in data
  x <- NULL
  grass_areas <- NULL
  grass_yld <- NULL
  try({grass_areas <- readGDX(gdx, "ov31_grass_area", format = "simplest")[, , list("type" = "level")]})
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

  } else {
    x <- "Disabled (no managed pastures)"
  }  
  return(x)
}
