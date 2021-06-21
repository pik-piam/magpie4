#' @title reportCostsAdaptationCrops
#' @description reports costs relevant to evaluate adaptation in crops
#'
#' @export
#'
#' @param gdx GDX file
#' @param type Type of reporting, either "annuity" or total "investments"
#' @return costs related to adaptation,  (million US$05/yr/tDM)
#' @author  Edna J. Molina Bacca
#' @importFrom magpiesets summationhelper
#' @importFrom magclass mbind
#' @examples
#' \dontrun{
#' x <- reportCostsAdaptationCrops(gdx)
#' }
#'
reportCostsAdaptationCrops <- function(gdx, type = "investment") {

  a <- costsAdaptationCrops(gdx, type = type, level = "regglo")
  getNames(a) <- paste0("Costs (Adaptation) |", getNames(a), " (million US$05/yr/tDM)")
  a <- summationhelper(x = a, sep = "+")
  x <- setNames(dimSums(a, dim = 3), "Costs (Adaptation) (million US$05/yr/tDM)")
  x <- mbind(x, a)

  return(x)
}
