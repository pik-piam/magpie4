#' @title reportTau2
#' @description reports Tau for managed pastures and croplands
#'
#' @export
#'
#' @param gdx GDX file
#' @return tau values as MAgPIE object for managed pastures (Index)
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' x <- reportTau2(gdx)
#' }
#'

reportTau2 <- function(gdx) {
  # x <- readGDX(gdx, "ov_tau", format = "first_found")[, , "level"]
  # if(length(getNames(x)) > 1){
  #   pastr <- collapseNames(x[,,"pastr"])
  #   area <- collapseNames(readGDX(gdx, "ov31_grass_area", format = "first_found")[, , "level"][,,"rainfed"][,,"pastr"])
  #   area <- gdxAggregate(gdx,area,to="reg",absolute = T)
  #   out1 <- superAggregate(pastr, aggr_type = "weighted_mean", level = "regglo", weight = area)
  #   getNames(out1) <- "Productivity|Landuse Intensity Indicator Tau Managed Pasture (Index)"
  #   
  #   crop <- collapseNames(x[,,"crop"])
  #   cr <- croparea(gdx, level = "reg", water_aggr = TRUE)
  #   out2 <- superAggregate(crop, aggr_type = "weighted_mean", level = "regglo", weight = cr)
  #   getNames(out2) <- "Productivity|Landuse Intensity Indicator Tau (Index)"
  #   return(mbind(out1, out2))
  # } else {
  #   x <- "Disabled (No separate grassland tau)"
  # }
  x <- "Disabled (testing mode)"
}
