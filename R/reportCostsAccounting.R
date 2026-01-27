#' @title reportCostsAccounting
#' @description reports MAgPIE costs including total investments
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return Costs accounting including total investments
#' @author Edna J. Molina Bacca
#' @importFrom magpiesets summationhelper
#' @importFrom magclass mbind
#' @examples
#' \dontrun{
#' x <- reportCostsAccounting(gdx)
#' }
#'
#' @section Cost accounting variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs Accounting | million US$2017/yr | Total cost accounting including investments
#' Costs Accounting\|+\|Land Conversion | million US$2017/yr | Investment costs for land conversion
#' Costs Accounting\|+\|Transport | million US$2017/yr | Transport cost investments
#' Costs Accounting\|+\|TC | million US$2017/yr | Technological change investment costs
#' @md
#' @export
reportCostsAccounting <- function(gdx, level = "regglo") {

  a <- costs(gdx, level = level, type = "investment", sum = FALSE)
  getNames(a) <- paste0("Costs Accounting|", getNames(a), " (million US$2017/yr)")
  a <- summationhelper(x = a, sep = "+")

  x <- mbind(setNames(dimSums(a, dim = 3), paste0("Costs Accounting (million US$2017/yr)")),
             a)

  return(x)
}
