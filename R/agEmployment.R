#' @title agEmployment
#' @description returns employment in crop+livestock production from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param type "absolute" for total number of people employed, "share" for share out of working age population
#' @param detail if TRUE, employment is disaggregated to crop products, livestock products and (if available) mitigation
#' measures, if FALSE only aggregated employment is reported
#' @param level spatial aggregation to report employment ("iso", "reg", "glo" or "regglo",
#' if type is "absolute" also "grid")
#' @param file a file name the output should be written to using write.magpie
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @return employment in agriculture as absolute value or as percentage of working age population
#' @author Debbora Leip
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- agEmployment(gdx)
#' }

agEmployment <- function(gdx, type = "absolute", detail = TRUE, level = "reg", file = NULL, dir = ".") {

  # CROP AND LIVESTOCK EMPLOYMENT
  agEmplProduction <- readGDX(gdx, "ov36_employment", select = list(type = "level"), react = "silent")
  years <- getYears(agEmplProduction)

  if (!is.null(agEmplProduction)) {
    # split into crop and livestock

    laborCostsKcr <- setNames(factorCosts(gdx, products = "kcr", level = "reg")[, , "labor_costs", drop = TRUE], "kcr")
    laborCostsKli <- setNames(factorCosts(gdx, products = "kli", level = "reg")[, , "labor_costs", drop = TRUE], "kli")
    shares <- mbind(laborCostsKcr, laborCostsKli) / collapseDim(laborCostsKcr + laborCostsKli)

    agEmplProduction <- agEmplProduction * shares

    # labor costs as disaggregation weight
    if (level %in% c("grid", "iso")) {
      weightKcr <- dimSums(laborCostsEndo(gdx, products = "kcr", level = level, dir = dir), dim = 3)
      weightKli <- dimSums(laborCostsEndo(gdx, products = "kli", level = level, dir = dir), dim = 3)
      weight <- mbind(setNames(weightKcr, "kcr"), setNames(weightKli, "kli"))
      wages <- readGDX(gdx, "p36_hourly_costs_iso")[, years, "scenario", drop = TRUE]
      hours <- readGDX(gdx, "f36_weekly_hours_iso")[, years, ]
      weight <- weight / gdxAggregate(gdx, hours * wages, to = level, absolute = FALSE, dir = dir)
    } else {
      weight <- NULL
    }
  
    # (dis-)aggregate
    agEmplProduction <- gdxAggregate(gdx, agEmplProduction, weight = weight,
                                     to = level, absolute = TRUE, dir = dir)
  }


  # EMPLOYMENT FROM MITIGATION MEASURES
  agEmplMitigation <- readGDX(gdx, "ov36_employment_maccs", select = list(type = "level"), react = "silent")

  if (!is.null(agEmplMitigation)) {
    # crop+livst production as disaggregation weight 
    if (level %in% c("grid", "iso")) {
      prodKcr <- production(gdx, products = "kcr", product_aggr = TRUE, level = level, dir = dir)
      prodKli <- production(gdx, products = "kli", product_aggr = TRUE, level = level, dir = dir)
      weight  <- magpiesort(prodKcr + prodKli)
      if (level == "iso") weight <- toolCountryFill(weight, fill = 0)
      wages <- readGDX(gdx, "p36_hourly_costs_iso")[, years, "scenario", drop = TRUE]
      hours <- readGDX(gdx, "f36_weekly_hours_iso")[, years, ]
      weight <- weight / gdxAggregate(gdx, hours * wages, to = level, absolute = FALSE, dir = dir)
      message(paste("Employment in mitigation is disaggregated by crop+livestock production,",
                    "and country level wages and hours worked."))
    } else {
      weight <- NULL
    }

    # (dis-)aggregate
    agEmplMitigation <- setNames(gdxAggregate(gdx, agEmplMitigation, weight = weight,
                                              to = level, absolute = TRUE, dir = dir), "maccs")
  }

  # COMBINE OUTPUTS
  if (!is.null(agEmplProduction)) {
    x <- mbind(agEmplProduction, agEmplMitigation)
    if (isFALSE(detail)) x <- setNames(dimSums(x, dim = 3),
                                       ifelse(!is.null(agEmplMitigation), "kcr_kli_maccs", "kcr_kli"))
  } else {
    x <- NULL
  }

  # CALCULATE EMPLOYMENT SHARE
  if (!is.null(x) && (type == "share")) {
    if (level == "grid") x <- NULL  # no population data on grid level
    if (level != "grid") {
        workingAge <- c("15--19", "20--24", "25--29", "30--34", "35--39", "40--44",
                        "45--49", "50--54", "55--59", "60--64")
        population <- dimSums(population(gdx, level = level, age = TRUE, dir = dir)[, , workingAge], dim = 3)
        x <- (x / population) * 100
    }
  }

  out(x, file)
}
