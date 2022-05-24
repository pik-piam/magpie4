#' @title grassland yields
#' @description Calculates grassland yields based on a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @return A MAgPIE object containing grassland yields values
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' x <- grassyld(gdx)
#' }
#'
grassyld <- function(gdx) {
  grass_yld <- NULL
  try({
    grass_yld <- readGDX(gdx, "i31_grass_yields", format = "simplest", react = "silent")
    grass_yld <- collapseNames(grass_yld)
    tau <- readGDX(gdx, "ov_tau", format = "simplest")
    tau <- gdxAggregate(gdx, tau, to = "cell", absolute = F)
    grass_yld <- grass_yld[, getYears(tau), ]
    grass_yld[, , "pastr"] <- (tau[, , "pastr.level"] / tau[, 1995 , "pastr.level"]) * grass_yld[, , "pastr"]
  }, silent = TRUE)
  return(grass_yld)
}