#' @title reportCostInputs
#' @description reports MAgPIE costs
#'
#' @export
#'
#' @param gdx GDX file
#'
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- reportCostInputs(gdx)
#' }
#' @importFrom magclass getNames

reportCostInputs <- function(gdx) {
  costAnnuity <- try(CostInputFactors(gdx, type = "overall", level = "regglo"), silent = TRUE)
  if ("try-error" %in% class(costAnnuity)) {
    message("Info only available for sticky cost implementation")
    return(NULL)
  }
  getNames(costAnnuity) <- "Costs|Overall input Costs (million US$05/yr)"
  return(costAnnuity)
}
