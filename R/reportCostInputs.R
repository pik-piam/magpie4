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
#' 
#'   \dontrun{
#'     x <- reportCostInputs(gdx)
#'   }
#' @importFrom magclass getNames

reportCostInputs <- function(gdx){
  cost_annuity <- try(CostInputFactors(gdx, type = "overall", level = "regglo"), silent = TRUE)
  if ("try-error" %in% class(cost_annuity)) {
    message("Info only available for sticky cost implementation")
    return(NULL)
  }
  getNames(cost_annuity) <- "Costs|Overall input Costs (million US$05/yr)"
  return(cost_annuity)
}