#' @title reportGrassStats
#' @description report evaluation values for pasture management implementation
#'
#' @export
#'
#' @param gdx GDX file
#' @return magpie object
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' x <- reportGrassStats(gdx)
#' }
#'
#'
#' @section Grass statistics variables:
#' Name | Unit | Meta
#' ---|---|---
#' Expansion costs\|+\|Pasture | mio USD17MER | Pasture expansion costs
#' Expansion costs\|+\|Range | mio USD17MER | Range expansion costs
#' Calibration factor\|+\|Pasture | 1 | Grassland calibration factor for pasture
#' Lambda\|+\|Pasture | 1 | Lambda parameter for pasture management
#' Productivity\|Yield (before calibration)\|+\|Pasture | tDM/ha | Uncalibrated pasture yield
#' @md

#'

reportGrassStats <- function(gdx) {

  x <- NULL
  grass_areas <- NULL
  grass_yld <- NULL
  grass_areas     <- readGDX(gdx, "ov31_grass_area", format = "simplest",  react = "silent")[, , list("type" = "level")]

  if(!is.null(grass_areas)){

  grass_yld       <- readGDX(gdx, "f31_grassl_yld", format = "simplest", react = "silent")
  grass_yld_reg <- collapseNames(gdxAggregate(gdx, grass_yld[,getYears(grass_areas),], weight= grass_areas, to = "regglo", absolute = FALSE)[,,"rainfed"])

  lamb       <- readGDX(gdx, "i31_lambda_grass", format = "simplest", react = "silent")
  lamb <- add_columns(lamb, "GLO", dim = 1, fill = 1)

  calib       <- readGDX(gdx, "i31_grass_calib", format = "simplest", react = "silent")
  calib_weight <- calib
  calib_weight[,,] <- 1
  calib_reg <- collapseNames(gdxAggregate(gdx, calib, weight=calib_weight, to = "regglo", absolute = FALSE))

  cost_exp     <- readGDX(gdx, "ov31_cost_grass_expansion", format = "simplest", react = "silent")[,,"level"]
  cost_exp_reg <- collapseNames(gdxAggregate(gdx, cost_exp, to = "regglo", absolute = T))

  x <- mbind(x, setNames(cost_exp_reg, paste0("Expansion costs|+|", reportingnames(getNames(cost_exp_reg, dim = 1)), " (mio USD17MER)")))
  x <- mbind(x, setNames(calib_reg[,getYears(x),], paste0("Calibration factor|+|", reportingnames(getNames(calib, dim = 1)), " (1)")))
  x <- mbind(x, setNames(lamb, paste0("Lambda|+|", reportingnames(getNames(lamb, dim = 1)), " (1)")))
  x <- mbind(x, setNames(grass_yld_reg, paste0("Productivity|Yield (before calibration)|+|", reportingnames(getNames(grass_yld_reg, dim = 1)), " (tDM/ha)")))

  return(x)
  } else {
    x <- "Disabled (no managed pastures)"
  }
}
