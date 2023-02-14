#' @title reportCostsProductionCrops
#' @description reports costs about production crops
#'
#' @export
#'
#' @param gdx GDX file
#' @param type Type of reporting, either "annuity" or total "investments"
#' @return costs related to crops production,  (million US$05/yr/tDM)
#' @author  Edna J. Molina Bacca
#' @importFrom magpiesets summationhelper
#' @importFrom magclass mbind
#' @examples
#' \dontrun{
#' x <- reportCostsProductionCrops(gdx)
#' }
#'
reportCostsProductionCrops <- function(gdx, type = "investment") {

  a <- costsProductionCrops(gdx, type = type, level = "regglo")
  getNames(a) <- paste0("Costs Crops production|", getNames(a), " (USD$05/yr)")

  a <- summationhelper(x = a, sep = "+")
  x <- setNames(dimSums(a[, , c("Variable (Crops)", "Capital (Crops)"), invert = TRUE], dim = 3),
                "Costs Crops production (USD$05/yr)")
  x <- mbind(x, a)

  return(x)
}
