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
    range_areas <- readGDX(gdx, "ov31_past_area")[, , "range.rainfed.level"]
  }, silent = T )
  try({
    sc_range <- read.magpie(file.path(dir, "soil_range_future.mz"))
  }, silent = T)

  if (!is.null(sc_range)) {
    if (!is.null(range_areas)) {
      range_areas_reg <- gdxAggregate(gdx, range_areas, to = "regglo", absolute = T)
      sc_range <- toolAggregate(sc_range, map_cell, from = "celliso", to = "iso")
      sc_range <- toolCountryFill(sc_range, fill = 0)
      sc_range_reg <- toolAggregate(sc_range, map_reg, from = "CountryCode", to = "RegionCode")
      sc_range_reg <- mbind(sc_range_reg, dimSums(sc_range_reg, dim = 1))

      sc_base_avg <- sc_range_reg / range_areas_reg[, getYears(sc_range), "range"]
      sc_base_avg[is.infinite(sc_base_avg) | is.nan(sc_base_avg)] <- 0

      x <- NULL
      x <- mbind(x, setNames(sc_base_avg, paste0("Resources|Soil Carbon|Grassland|Range|Density (tC per ha)")))
      x <- mbind(x, setNames(sc_range, paste0("Resources|Soil Carbon|Grassland|Range|Total (tC)")))
      
    } else {
      message("Disabled (dissagregation must be run first) ", appendLF = FALSE)
    }
  } else {
    message("Disabled (no managed pastures) ", appendLF = FALSE)
  }
  return(x)
}
