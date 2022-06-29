#' @title reportCostsWithoutIncentives
#' @description reports Costs Without Incentives
#'
#' @export
#'
#' @param gdx GDX file
#' @return magpie object
#' @author David Chen
#' @examples
#' \dontrun{
#' x <- reportCostsWithoutIncentives(gdx)
#' }
#'
#'
reportCostsWithoutIncentives <- function(gdx) {

  costWoInc <- CostsWithoutIncentives(gdx, level = "regglo")
  getNames(costWoInc) <- paste0("Costs Without Incentives",
                                " (million US$05/yr)")

  return(costWoInc)
}
