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
  grassYields <- NULL
  x <- NULL
  grassAreas <- NULL
  grassYield <- NULL

  try({grassYield <- grassyld(gdx)})
  try({
    grassAreas <- readGDX(gdx, "ov31_grass_area", format = "simplest",  react = "silent")[, , list("type" = "level")]
  })

  if(!is.null(grassYield)) {
    grassYields <- gdxAggregate(gdx, grassYield, weight = grassAreas, to = "regglo", absolute = F)
    x <- setNames(grassYields, paste0("Productivity|Yield|+|", reportingnames(getNames(grassYields)), " (t DM/ha)"))
  } else {
    x <- "Disabled (No separate grassland yields)"
  }
  return(x)
}
