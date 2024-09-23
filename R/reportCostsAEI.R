#' @title reportCostsAEI
#' @description reports MAgPIE AEI costs
#'
#' @param gdx GDX file
#' @return magpie object containing AEI costs
#' @author Felicitas Beier
#' @examples
#'
#'   \dontrun{
#'     x <- reportCostsAEI(gdx)
#'   }
#'

reportCostsAEI <- function(gdx) {

  AEI_costs <- CostsAEI(gdx, level = "regglo")

  getNames(AEI_costs)<-"Costs|AEI"
  getNames(AEI_costs) <- paste0(getNames(AEI_costs)," (million US$2017/yr)")

  return(AEI_costs)
}
