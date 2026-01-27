#' @title reportCostCapitalInvestment
#' @description reports MAgPIE capital investments
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- reportCostCapitalInvestment(gdx)
#' }
#'
#' @section Capital investment cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs\|Capital Investments | million US$2017 | Capital investments (sticky cost implementation)
#' @md
#' @importFrom magclass getNames
#'
#' @export
reportCostCapitalInvestment <- function(gdx, level = "regglo") {
  # Capital stocks used in croland per region
  x <- try(CostCapital(gdx, level = level, type = "investment"), silent = TRUE)
  if ("try-error" %in% class(x)) {
    message("Info only available for sticky cost implementation")
    return(NULL)
  }
  getNames(x) <- "Costs|Capital Investments (million US$2017)"
  return(x)
}
