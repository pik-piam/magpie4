#' @title reportCostsWithoutIncentives
#' @description reports Costs Without Incentives
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @return magpie object
#' @author David Chen
#' @examples
#' \dontrun{
#' x <- reportCostsWithoutIncentives(gdx)
#' }
#'
#'
reportCostsWithoutIncentives <- function(gdx,level = "regglo") {

  costWoInc <- CostsWithoutIncentives(gdx, level = level)
  getNames(costWoInc) <- paste0("Costs Without Incentives",
                                " (million US$2017/yr)")

  return(costWoInc)
}
