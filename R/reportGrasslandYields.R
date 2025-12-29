#' @title reportGrasslandYields
#' @description reportGrasslandYields
#' 
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#' 
#' @param gdx GDX file
#' @return yield as MAgPIE object (Mt DM/ha)
#' @author Marcos Alves
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGrasslandYields(gdx)
#'   }
#' 
#'
#' @section Grassland yield variables:
#' Name | Unit | Meta
#' ---|---|---
#' Productivity\|Yield\|+\|Pasture | t DM/ha | Pasture grassland yield
#' Productivity\|Yield\|+\|Range | t DM/ha | Range grassland yield
#' @md


reportGrasslandYields <- function(gdx) {
  grass_yields <- NULL
  x <- NULL
  grass_areas <- NULL
  grass_yld <- NULL
  
  try({grass_yld <- grassyld(gdx)})
  try({grass_areas <- readGDX(gdx, "ov31_grass_area", format = "simplest",  react = "silent" )[, , list("type" = "level")]})
  
  if(!is.null(grass_yld)) {
    grass_yields <- gdxAggregate(gdx, grass_yld, weight = grass_areas, to = "regglo", absolute = F)
    x <- setNames(grass_yields, paste0("Productivity|Yield|+|", reportingnames(getNames(grass_yields)), " (t DM/ha)"))
    return(x)
  } else {
    x <- "Disabled (No separate grassland yields)"
  }
} 
