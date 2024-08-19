#' @title reportCostCapitalInvestment
#' @description reports MAgPIE capital investments
#'
#' @export
#'
#' @param gdx GDX file
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- reportCostCapitalInvestment(gdx)
#' }
#' @importFrom magclass getNames

reportCostCapitalInvestment <- function(gdx) {
  # Capital stocks used in croland per region
  x <- try(CostCapital(gdx, level = "regglo", type = "investment"), silent = TRUE)
  if ("try-error" %in% class(x)) {
    message("Info only available for sticky cost implementation")
    return(NULL)
  }
  getNames(x) <- "Costs|Capital Investments (million US$17)"
  return(x)
}
