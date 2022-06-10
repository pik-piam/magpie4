#' @title totalHoursWorked
#' @description returns total hours worked per year in crop+livestock production from MAgPIE results
#' @param gdx GDX file
#' @param level spatial aggregation to report employment ("reg", "glo", or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @return total hours worked in agriculture per year
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' x <- totalHoursWorked(gdx)
#' }


totalHoursWorked <- function(gdx, level = "reg", file = NULL) {

  agEmployment <- agEmployment(gdx, type = "absolute", level = "reg")
  weeklyHours  <- readGDX(gdx, "f36_weekly_hours", react = "silent")
  weeksInYear  <- readGDX(gdx, "s36_weeks_in_year", react = "silent")

  if (!is.null(weeklyHours)) {
    x <- agEmployment * weeklyHours[, getYears(agEmployment), ] * weeksInYear
    x <- superAggregate(x, level = level, aggr_type = "sum")
  } else { # for MAgPIE versions before implementation of employment return 0
    x <- agEmployment
  }

  out(x, file)
}
