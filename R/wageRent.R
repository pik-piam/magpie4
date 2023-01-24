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

  # get wage difference, but correcting baseline wages for productivity gain (as that
  # part is an actual higher cost paied for labor)
  wage <- readGDX(gdx, "pm_hourly_costs")
  productivityGain <- readGDX(gdx, "pm_productivity_gain_from_wages")
  wage[, , "baseline"] <- wage[, , "baseline"] * productivityGain
  wageDiff <- wage[, , "scenario", drop = TRUE] - wage[, , "baseline", drop = TRUE]

  # get total hours worked
  employment  <- agEmployment(gdx = gdx, detail = FALSE, level = "reg")
  weeklyHours <- readGDX(gdx, "f36_weekly_hours")[, getYears(wageDiff), ]
  weeksInYear <- readGDX(gdx, "s36_weeks_in_year")
  totalHours  <- employment * weeklyHours * weeksInYear 

  # calculate wage rent as employment * weekly hours * weeks in year* wage difference
  wageRent <- setNames(totalHours * wageDiff, "wage_rent")
  wageRent <- superAggregate(wageRent, aggr_type = "sum", level = level)

  return(wageRent)
}
