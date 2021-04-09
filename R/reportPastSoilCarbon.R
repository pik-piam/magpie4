#' @title reportPastSoilCarbon
#' @description reports pasture soil carbon
#'
#' @export
#'
#' @param gdx GDX file
#' @return Soil carbon values as magpie objetc
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' x <- reportPastSoilCarbon(gdx)
#' }
#'
reportPastSoilCarbon <- function(gdx) {
  
  # read in data
  soilc_cg_target <- readGDX(gdx, "ov31_soilc_target", format = "simplest")[, , list("type" = "level")]
  past_ha_c <- readGDX(gdx, "ov31_past_area", format = "simplest")[, , list("past_mngt" = "cont_grazing", "type" = "level", "w" = "rainfed")]
  soilc_cg_target <- soilc_cg_target/past_ha_c
  soilc_cg_target[is.nan(soilc_cg_target)] <- 0
  soilc_cg_target[is.infinite(soilc_cg_target)] <- 0
  soilc_cg_target_reg <- gdxAggregate(gdx, soilc_cg_target, to = "regglo", weight = past_ha_c, absolute = F)
  soilc_cg_change_reg <- soilc_cg_target_reg - soilc_cg_target_reg[,1,]
  getYears(soilc_cg_change_reg) <- getYears(soilc_cg_target_reg)
  
  # aggreate and rename
  x <- NULL
  x <- mbind(x, setNames(soilc_cg_target_reg, "Resources|Soil Carbon Stocks|Pasture|Continuous grazing|Density (tC per ha)"))
  x <- mbind(x, setNames(soilc_cg_change_reg, "Resources|Soil Carbon Change|Pasture|Continuous grazing|Density (tC per ha)"))

  return(x)
}
