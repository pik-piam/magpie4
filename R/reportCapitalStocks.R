#' @title reportCostCapitalStocks
#' @description reports MAgPIE capital stocks
#'
#' @export
#'
#' @param gdx GDX file
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- reportCostCapitalStocks(gdx)
#' }
#' @importFrom magclass getNames
#'
#' @section Capital stock variables:
#' Name | Unit | Meta
#' ---|---|---
#' Capital Stocks\|Arable farm capital | million US$2017 | Capital stocks used in cropland (sticky cost implementation)
#' @md

reportCostCapitalStocks <- function(gdx) {

  # Capital stocks used in croland per region
  x <- try(CostCapital(gdx, level = "regglo", type = "stocks"), silent = TRUE)
  if ("try-error" %in% class(x)) {
    message("Info only available for sticky cost implementation")
    return(NULL)
  }
  getNames(x) <- "Capital Stocks|Arable farm capital (million US$2017)"
  return(x)
}
