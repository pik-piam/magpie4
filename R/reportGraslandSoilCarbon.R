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
  
  map <- toolGetMapping(type = "regional", name = "clustermapping.csv")
  
  grass_areas <- NULL
  sc_range <- NULL
  sc_pastr <- NULL
  sc_grassland <- NULL
  sc_net <- NULL
  x <- NULL

  try({
    grass_areas <- collapseNames(readGDX(gdx, "ov31_past_area")[, , "rainfed.level"])
  }, silent = T )
  try({
    sc_range <- read.magpie(file.path(dir, "soil_range_future.mz"))
    sc_pastr <- read.magpie(file.path(dir, "soil_pastr_future.mz"))
    sc_grassland <- read.magpie(file.path(dir, "grassland_soil_carbon.mz"))
    sc_net <- read.magpie(file.path(dir, "net_management_change_range.mz"))
    sc_net0 <- read.magpie(file.path(dir, "net_management_change_range0.mz"))
  }, silent = T)

  if (!is.null(grass_areas)) {
    if (!is.null(sc_range)) {
      grass_areas <- gdxAggregate(gdx, grass_areas, to = "regglo", absolute = T)

      sc_range_reg <- toolAggregate(sc_range, map, from = "cell", to = "region")
      sc_range_reg <- mbind(sc_range_reg, dimSums(sc_range_reg, dim = 1))
      
      sc_pastr_reg <- toolAggregate(sc_pastr, map, from = "cell", to = "region")
      sc_pastr_reg <- mbind(sc_pastr_reg, dimSums(sc_pastr_reg, dim = 1))
      
      sc_total <- setNames(mbind(sc_pastr_reg, sc_range_reg), c("pastr", "range"))
      
      sc_total_avg <- sc_total / grass_areas[, getYears(sc_total), c("pastr", "range") ]
      sc_total_avg[is.infinite(sc_total_avg) | is.nan(sc_total_avg)] <- 0
      sc_total_avg_t <- dimSums(sc_total, dim =3) / dimSums(grass_areas[, getYears(sc_total), c("pastr", "range") ], dim = 3)
      sc_total_avg_t[is.infinite(sc_total_avg_t) | is.nan(sc_total_avg_t)] <- 0
      
      sc_net_reg <- toolAggregate(sc_net, map, from = "cell", to = "region")
      sc_net_reg <- mbind(sc_net_reg, dimSums(sc_net_reg, dim = 1))
      
      sc_net0_reg <- toolAggregate(sc_net0, map, from = "cell", to = "region")
      sc_net0_reg <- mbind(sc_net0_reg, dimSums(sc_net0_reg, dim = 1))
      
      x <- NULL
      x <- mbind(x, setNames(sc_total_avg, paste0("Resources|Soil Carbon|Grassland|+|",reportingnames(getNames(sc_total_avg, dim = 1)),"|Density (tC per ha)")))
      x <- mbind(x, setNames(dimSums(sc_total_avg_t, dim = 3), paste0("Resources|Soil Carbon|Grassland|Density (tC per ha)")))
      x <- mbind(x, setNames(sc_total, paste0("Resources|Soil Carbon|Grassland|+|",reportingnames(getNames(sc_total, dim = 1)),"|Total (MtC)")))
      x <- mbind(x, setNames(dimSums(sc_total, dim = 3), paste0("Resources|Soil Carbon|Grassland|Total (tC)")))
      x <- mbind(x, setNames(dimSums(sc_net_reg, dim = 3), paste0("Resources|Soil Carbon Change|Rangelands|Management related (MtC)")))
      x <- mbind(x, setNames(dimSums(sc_net0_reg, dim = 3), paste0("Resources|Soil Carbon Change|Rangelands|Management related 0 Baseline (MtC)")))
      
    } else {
      print("Disabled (dissagregation must be run first) ")
    }
  } else {
    print("Disabled (no managed pastures) ")
  }
  return(x)
}
