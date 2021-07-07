#' @title FactorCosts
#' @description reads factor costs for livestock, residues or pasture entering
#' the objective function from a MAgPIE gdx file, and splits into labor costs
#' and capital costs in case the gdx file comes from a run using the sticky
#' factor costs realization
#'
#' @export
#'
#' @param gdx GDX file
#' @param products products for which factor costs should be reported ("kli",
#' "kres", "fish", or "pastures")
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @return MAgPIE object containing factor costs [million US$05]
#' @author Debbora Leip
#' @importFrom gdx readGDX out
#' @importFrom magclass add_dimension getNames mbind
#' @importFrom luscale superAggregate
#' @importFrom madrat toolAggregate
#' @examples
#' \dontrun{
#' x <- FactorCosts(gdx)
#' }
#'
FactorCosts <- function(gdx, products = "kli", file = NULL, level = "regglo") {
  if (products == "kli") products <- findset("kli")
  else if (products == "kres") products <- findset("kres")

  factor_costs <- readGDX(gdx, "ov_cost_prod", react = "silent",
                          format = "first_found", select = list(type = "level"))[, , products]

  cap_share_reg_parameters <- readGDX(gdx, "f38_reg_parameters", react = "silent", format = "first_found")
  cap_historical_share <- readGDX(gdx, "f38_historical_share", react = "silent", format = "first_found")
  mapping <- readGDX(gdx, "i_to_iso", react = "silent", format = "first_found")
  gdp_pc_ppp <- toolAggregate(readGDX(gdx, "im_gdp_pc_ppp_iso", react = "silent", format = "first_found"),
                                      rel = mapping, from = "iso", to = "i", weight = NULL)

  if (!is.null(cap_historical_share)) {
    share_calibration <- cap_historical_share[, 2010, ] - (as.double(cap_share_reg_parameters[, , "slope"]) *
                                     log10(gdp_pc_ppp[, 2010, ]) + as.double(cap_share_reg_parameters[, , "intercept"]))
    capital_share <- cap_historical_share[, seq(1995, 2100, 5), ]
    capital_share[, seq(2015, 2100, 5), ] <- share_calibration + as.double(cap_share_reg_parameters[, , "intercept"]) +
                         as.double(cap_share_reg_parameters[, , "slope"]) * log10(gdp_pc_ppp[, seq(2015, 2100, 5), ])

    factor_costs <- add_dimension(factor_costs, add = "cost_type", nm = "factor_costs")
    capital_costs <- factor_costs * capital_share[, getYears(factor_costs), ]
    labor_costs <- factor_costs - capital_costs
    getNames(capital_costs, dim = 1) <- "capital_costs"
    getNames(labor_costs, dim = 1) <- "labor_costs"

    factor_costs <- mbind(capital_costs, labor_costs, factor_costs)
  }

  factor_costs <- superAggregate(factor_costs, aggr_type = "sum", level = level)

  out(factor_costs, file)
}
