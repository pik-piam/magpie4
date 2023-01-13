#' @title wageRent
#' @description calculates wage rent for exogenous wage scenarios
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, reg, glo or regglo
#' @author Debbora Leip
#' @importFrom luscale superAggregate

#' @examples
#' \dontrun{
#' x <- wageRent(gdx)
#' }
#'
wageRent <- function(gdx, file = NULL, level = "regglo") {

  empl <- agEmployment(gdx = gdx, detail = FALSE, level = "reg")

  wage <- readGDX(gdx, "pm_hourly_costs")
  wageDiff <- wage[, , "scenario", drop = TRUE] - wage[, , "baseline", drop = TRUE]

  wageRent <- setNames(empl * wageDiff, "wage_rent")

  wageRent <- superAggregate(wageRent, aggr_type = "sum", level = level)

  return(wageRent)
}
