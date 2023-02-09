#' @title reportCostsAccounting
#' @description reports MAgPIE costs including total investments
#'
#' @export
#'
#' @param gdx GDX file
#' @return Costs accounting including total investments
#' @author Edna J. Molina Bacca
#' @importFrom magpiesets summationhelper
#' @importFrom magclass mbind
#' @examples
#' \dontrun{
#' x <- reportCostsAccounting(gdx)
#' }
#'
reportCostsAccounting <- function(gdx) {

  a <- costs(gdx, level = "regglo", type = "investment", sum = FALSE)
  getNames(a) <- paste0("Costs Accounting|", getNames(a), " (million US$05/yr)")
  a <- summationhelper(x = a, sep = "+")

  x <- NULL
  x <- mbind(x, setNames(dimSums(a, dim = 3), paste0("Costs Accounting (million US$05/yr)")))
  x <- mbind(x, a)


  return(x)
}
