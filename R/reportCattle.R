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
  past_ha_c <- NULL
  past_yld_m <- NULL
  past_ha_m <- NULL
  past_yld_c <- NULL
  # read in data
  try({past_ha_c <- readGDX(gdx, "ov31_past_area", format = "simplest")[, , list("past_mngt" = "cont_grazing", "type" = "level", "w" = "rainfed")]})
  try({past_yld_c <- readGDX(gdx, "ov_past_yld", format = "simplest")[, , list("past_mngt" = "cont_grazing", "type" = "level", "w" = "rainfed")]})
  try({past_yld_m <- readGDX(gdx, "ov_past_yld", format = "simplest")[, , list("past_mngt" = "mowing", "type" = "level", "w" = "rainfed")]})
  try({past_ha_m <- readGDX(gdx, "ov31_past_area", format = "simplest")[, , list("past_mngt" = "mowing", "type" = "level", "w" = "rainfed")]})

  if (!any(c(is.null(past_ha_c), is.null(past_yld_m),is.null(past_ha_m),is.null(lsu_ha)))) {
  total_grazing_cattle_c <- past_ha_c * past_yld_c / (8.9 * 365 / 1000) # (lsu equivalent anual consumption in tDM)
  total_mowing_cattle <- past_yld_m * past_ha_m / (8.9 * 365 / 1000) # (lsu equivalent anual consumption in tDM)
  lsu_ha_m <- total_mowing_cattle/past_ha_m
  lsu_ha_m[is.nan(lsu_ha_m)] <- 0

  # aggregate and add global
  total_grazing_cattle_c_reg <- gdxAggregate(gdx, total_grazing_cattle_c, to = "regglo", absolute = T)
  total_mowing_cattle_reg <- gdxAggregate(gdx, total_mowing_cattle, to = "regglo", absolute = T)
  lsu_ha_reg <- gdxAggregate(gdx, lsu_ha, to = "regglo", weight = past_ha_c, absolute = F)
  lsu_ha_m_reg <- gdxAggregate(gdx, lsu_ha_m, to = "regglo", weight = past_ha_m, absolute = F)
  past_ha_c_reg <- gdxAggregate(gdx, past_ha_c, to = "regglo", absolute = T)
  past_ha_m_reg <- gdxAggregate(gdx, past_ha_m, to = "regglo", absolute = T)

  # aggreate and rename
  x <- NULL
  x <- mbind(x, setNames(lsu_ha_reg, "Stock density|+|Cattle|Rangelands (Lsu per ha)"))
  x <- mbind(x, setNames(lsu_ha_m_reg, "Stock density|+|Cattle|Managed Pastures (Lsu per ha)"))
  x <- mbind(x, setNames(total_grazing_cattle_c_reg, "Total lsu|+|Cattle|Rangelands (millions)"))
  x <- mbind(x, setNames(total_mowing_cattle_reg, "Total lsu|+|Cattle|Managed Pastures (millions)"))
  x <- mbind(x, setNames(past_ha_c_reg, paste0("Management|", reportingnames("past"), "|+|Rangelands", " (million ha)")))
  x <- mbind(x, setNames(past_ha_m_reg, paste0("Management|", reportingnames("past"), "|+|Managed Pastures", " (million ha)")))

  return(x)
  }
}
