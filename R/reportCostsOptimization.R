#' @title reportCostsOptimization
#' @description reports MAgPIE costs
#'
#' @export
#'
#' @param gdx GDX file
#' @return reports the costs entering the optimization
#' @author Edna J. Molina Bacca
#' @importFrom magpiesets summationhelper
#' @importFrom magclass mbind
#' @examples
#' \dontrun{
#' x <- reportCostsOptimization(gdx)
#' }
#'
reportCostsOptimization <- function(gdx) {

  a <- costsOptimization(gdx, level = "regglo", type = "annuity", sum = FALSE)
  getNames(a) <- paste0("Costs Optimization|", getNames(a), " (million US$05/yr)")
  a <- summationhelper(x = a, sep = "+")

  x <- NULL
  x <- mbind(x, setNames(dimSums(a, dim = 3), paste0("Costs Optimization (million US$05/yr)")))
  x <- mbind(x, a)


  return(x)
}
