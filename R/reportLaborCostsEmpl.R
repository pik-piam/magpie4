#' @title reportLaborCostsEmpl
#' @description reports MAgPIE labor costs that go into employment calculation
#'
#' @param gdx GDX file
#' @return magpie object with labor costs
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportLaborCostsEmpl(gdx)
#'   }
#'
reportLaborCostsEmpl <- function(gdx) {

  if (!is.null(readGDX(gdx, "ov36_employment", select = list(type = "level"), react = "silent"))) {
    # crop and livestock labor costs are also reported under Costs Optimization|Input Factors|Labor costs|+|xx
    # maccs labor costs are also reported under Costs Optimization|MACCS|+|Labor costs
    # the non-maagpie labor costs don't go into the optimizaiton and are therefore not reported elsewhere
    costsCrops <- factorCosts(gdx, products = "kcr", level = "regglo")[, , "labor_costs", drop = TRUE]
    costsLivst <- factorCosts(gdx, products = "kli", level = "regglo")[, , "labor_costs", drop = TRUE]
    costsNonMagpie <- superAggregate(readGDX(gdx, "p36_nonmagpie_labor_costs"), level = "regglo", aggr_type = "sum")
    costsMACCS <- costsMACCS(gdx, level = "regglo")[, , "labor"]

    out <- costsCrops + costsLivst + costsNonMagpie
    if (!is.null(costsMACCS)) out <- out + costsMACCS

    getNames(out) <- "Labor costs linked to employment (million US$05/yr)"
  } else {
    out <- NULL
  }

  return(out)
}
