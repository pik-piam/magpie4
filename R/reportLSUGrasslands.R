#' @title reportLSUGrasslands
#' @description reportLSUGrasslands
#' 
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#' 
#' @param gdx GDX file
#' @return Livestock eq. denstity as a magpie object (Mt DM/ha) (1 LSU eq. = 8.9 kg DM/day)
#' @author Marcos Alves
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGrasslandYields(gdx)
#'   }
#' 

reportLSUGrasslands <- function(gdx) {
  lsu_cell <- NULL
  x <- NULL
  grass_areas <- NULL
  
  lsu_cell <- readGDX(gdx, "ov31_lsu_ha", format = "simplest",  react = "silent" )[, , "level"]
  grass_areas <- readGDX(gdx, "ov31_grass_area", format = "simplest",  react = "silent" )[, , "range.level"]

  if(!is.null(lsu_cell)) {
    lsu_reg <- gdxAggregate(gdx, lsu_cell, weight = grass_areas, to = "regglo", absolute = F)
    x <- setNames(lsu_reg, paste0("Productivity|Livestock eq. density|+|", reportingnames("range"), " (LSU eq./ha)"))
    return(x)
  } else {
    x <- "Disabled (No LSU management)"
  }
} 
