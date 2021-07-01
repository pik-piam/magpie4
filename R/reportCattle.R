#' @title reportCattle
#' @description reports cattle related numbers
#'
#' @export
#'
#' @param gdx GDX file
#' @return Cattle values as magpie objetc
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' x <- reportCattle(gdx)
#' }
#'
#'


reportCattle <- function(gdx) {

  x <- NULL
  range_ha <- NULL
  pastr_yld <- NULL
  pastr_ha <- NULL
  range_yld <- NULL
  range_yld <- NULL
  # read in data
  try({range_ha <- readGDX(gdx, "ov31_past_area", format = "simplest")[, , list("past_mngt" = "range", "type" = "level", "w" = "rainfed")]})
  try({range_yld <- readGDX(gdx, "ov_past_yld", format = "simplest")[, , list("past_mngt" = "range", "type" = "level", "w" = "rainfed")]})
  try({pastr_yld <- readGDX(gdx, "ov_past_yld", format = "simplest")[, , list("past_mngt" = "pastr", "type" = "level", "w" = "rainfed")]})
  try({pastr_ha <- readGDX(gdx, "ov31_past_area", format = "simplest")[, , list("past_mngt" = "pastr", "type" = "level", "w" = "rainfed")]})
  try({grass_areas <- readGDX(gdx, "ov31_past_area", format = "simplest")[, , list("type" = "level", "w" = "rainfed")]})

  if (!all(c(is.null(range_ha), is.null(pastr_yld),is.null(pastr_ha),is.null(range_yld), is.null(grass_areas)))) {
  total_range_lsus <- range_ha * range_yld / (8.9 * 365 / 1000) # (lsu equivalent anual consumption in tDM)
  total_pastr_lsus <- pastr_yld * pastr_ha / (8.9 * 365 / 1000) # (lsu equivalent anual consumption in tDM)
  lsu_ha_pastr <- total_pastr_lsus/pastr_ha
  lsu_ha_pastr[is.nan(lsu_ha_pastr)] <- 0
  lsu_ha_range <- total_range_lsus/range_ha
  lsu_ha_range[is.nan(lsu_ha_range)] <- 0
  grass_areas <- collapseNames(grass_areas)
  
  # aggregate and add global
  total_range_lsus_reg <- gdxAggregate(gdx, total_range_lsus, to = "regglo", absolute = T)
  total_pastr_lsus_reg <- gdxAggregate(gdx, total_pastr_lsus, to = "regglo", absolute = T)
  lsu_ha_reg <- gdxAggregate(gdx, lsu_ha_range, to = "regglo", weight = range_ha, absolute = F)
  lsu_ha_pastr_reg <- gdxAggregate(gdx, lsu_ha_pastr, to = "regglo", weight = pastr_ha, absolute = F)
  range_ha_reg <- gdxAggregate(gdx, range_ha, to = "regglo", absolute = T)
  pastr_ha_reg <- gdxAggregate(gdx, pastr_ha, to = "regglo", absolute = T)
  grass_areas_reg <- gdxAggregate(gdx, grass_areas, to = "regglo", absolute = T)
    
  # aggreate and rename
  x <- NULL
  x <- mbind(x, setNames(lsu_ha_reg, "Stock density|+|Cattle|Rangelands (Lsu per ha)"))
  x <- mbind(x, setNames(lsu_ha_pastr_reg, "Stock density|+|Cattle|Managed Pastures (Lsu per ha)"))
  x <- mbind(x, setNames(total_range_lsus_reg, "Total lsu|+|Cattle|Rangelands (millions)"))
  x <- mbind(x, setNames(total_pastr_lsus_reg, "Total lsu|+|Cattle|Managed Pastures (millions)"))
  x <- mbind(x, setNames(range_ha_reg, paste0("Management|", reportingnames("past"), "|+|Rangelands ", " (million ha)")))
  x <- mbind(x, setNames(pastr_ha_reg, paste0("Management|", reportingnames("past"), "|+|Managed Pastures ", " (million ha)")))
  x <- mbind(x, setNames(grass_areas_reg[,,"pastr"],paste0("Resources|Land Cover|+|Managed Pastures (million ha)")))
  x <- mbind(x, setNames(grass_areas_reg[,,"pastr"],paste0("Resources|Land Cover|+|Rangelands (million ha)")))
  

  return(x)
  }
}
