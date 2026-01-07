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
#' @section Costs without incentives variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs Accounting\|Costs without incentives | million US$2017/yr | Total costs excluding policy incentives
#' @md

#'
reportCostsWithoutIncentives <- function(gdx,level = "regglo") {

  costWoInc <- CostsWithoutIncentives(gdx, level = level)
  getNames(costWoInc) <- paste0("Costs Accounting|Costs without incentives",
                                " (million US$2017/yr)")

  return(costWoInc)
}
