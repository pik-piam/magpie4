#' @title reportGraslandSoilCarbon
#' @description reports cattle related numbers
#'
#' @export
#'
#' @param gdx GDX file
#' @param dir dir
#' @param spamfiledirectory old spamfiledirectory
#' @return Cattle values as magpie objetc
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' x <- reportGraslandSoilCarbon(gdx)
#' }
#' @importFrom madrat toolGetMapping getConfig toolCountryFill

reportGraslandSoilCarbon <- function(gdx, dir = ".", spamfiledirectory = "") {
  
  dir <- dirname(gdx)
  
  map_cell <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")
  map_reg <- toolGetMapping(type = "regional", name = getConfig("regionmapping"))

  range_areas <- NULL
  sc_range <- NULL
  x <- NULL

  try({
    grass_areas <- collapseNames(readGDX(gdx, "ov31_past_area")[, , "rainfed.level"])
  }, silent = T )
  try({
    sc_range <- read.magpie(file.path(dir, "soilc_range_future.mz"))
    sc_pastr <- read.magpie(file.path(dir, "soilc_pastr_future.mz"))
    sc_grassland <- read.magpie(file.path(dir, "grassland_soil_carbon.mz"))
  }, silent = T)

  if (!is.null(grass_areas)) {
    if (!is.null(sc_range)) {
      grass_areas <- gdxAggregate(gdx, grass_areas, to = "regglo", absolute = T)
      
      sc_range <- toolAggregate(sc_range, map_cell, from = "celliso", to = "iso")
      sc_range <- toolCountryFill(sc_range, fill = 0)
      sc_range_reg <- toolAggregate(sc_range, map_reg, from = "CountryCode", to = "RegionCode")
      sc_range_reg <- mbind(sc_range_reg, dimSums(sc_range_reg, dim = 1))
      
      sc_pastr <- toolAggregate(sc_pastr, map_cell, from = "celliso", to = "iso")
      sc_pastr <- toolCountryFill(sc_pastr, fill = 0)
      sc_pastr_reg <- toolAggregate(sc_pastr, map_reg, from = "CountryCode", to = "RegionCode")
      sc_pastr_reg <- mbind(sc_pastr_reg, dimSums(sc_pastr_reg, dim = 1))
      
      sc_base <- setNames(mbind(sc_pastr_reg, sc_range_reg), c("pastr", "range"))
      
      sc_base_avg <- sc_base / grass_areas[, getYears(sc_base), c("pastr", "range") ]
      sc_base_avg[is.infinite(sc_base_avg) | is.nan(sc_base_avg)] <- 0
      sc_base_avg_t <- dimSums(sc_base, dim =3) / dimSums(grass_areas[, getYears(sc_base), c("pastr", "range") ], dim = 3)
      sc_base_avg_t[is.infinite(sc_base_avg_t) | is.nan(sc_base_avg_t)] <- 0
      
      x <- NULL
      x <- mbind(x, setNames(sc_base_avg, paste0("Resources|Soil Carbon|Grassland|+|",reportingnames(getNames(grass_areas, dim = 1)),"|Density (tC per ha)")))
      x <- mbind(x, setNames(dimSums(sc_base_avg_t, dim = 3), paste0("Resources|Soil Carbon|Grassland|Density (tC per ha)")))
      x <- mbind(x, setNames(sc_base, paste0("Resources|Soil Carbon|Grassland|+|",reportingnames(getNames(grass_areas, dim = 1)),"|Total (MtC)")))
      x <- mbind(x, setNames(dimSums(sc_base, dim = 3), paste0("Resources|Soil Carbon|Grassland|Total (tC)")))
      
    } else {
      print("Disabled (dissagregation must be run first) ")
    }
  } else {
    print("Disabled (no managed pastures) ")
  }
  return(x)
}
