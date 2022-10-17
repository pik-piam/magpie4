#' @title relativeHourlyLaborCosts
#' @description calculates labor costs per ag. worker in relation to GDP pc
#' @param gdx GDX file
#' @param level spatial aggregation to report ("iso", "reg", "glo", or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @return labor costs per ag. worker in relation to GDP pc
#' @author Debbora Leip
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- relativeHourlyLaborCosts(gdx)
#' }

relativeHourlyLaborCosts <- function(gdx, level = "reg", file = NULL) {

  hourlyLaborCosts <- hourlyLaborCosts(gdx, level = "iso")

  if (!is.null(hourlyLaborCosts)) {
    weeklyHours <- readGDX(gdx, "f36_weekly_hours_iso", react = "silent")
    yearlyLaborCosts <- 52.1429 * weeklyHours[, getYears(hourlyLaborCosts)] * hourlyLaborCosts
    gdpPCmer <- readGDX(gdx, "im_gdp_pc_mer_iso")
    relLaborCosts <- yearlyLaborCosts / gdpPCmer[, getYears(hourlyLaborCosts), ] * 100

    if (level == "iso") {
      out <- relLaborCosts
    }
    else {
      # aggregate to regional level using historic ag. empl. as weight
      i2iso <- readGDX(gdx, "i_to_iso")
      agEmplIso <- readGDX(gdx, "f36_historic_ag_empl", react = "silent")[, 2010, ]
      relLaborCosts <- toolAggregate(relLaborCosts, rel = i2iso, weight = agEmplIso, from = "iso", to = "i")

      # aggregate to global level using modeled regional ag. empl. as weight
      agEmpl <- readGDX(gdx, "ov36_employment", select = list(type = "level"), react = "silent")
      out <- superAggregate(data = relLaborCosts[, getYears(agEmpl), ], aggr_type = "weighted_mean", level = level, weight = agEmpl)
    }
  } else {
    out <- NULL
  }

  out(out, file)
}
