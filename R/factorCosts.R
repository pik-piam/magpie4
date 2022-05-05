#' @title factorCosts
#' @description reads factor costs for crops, livestock, residues or pasture
#' entering the objective function from a MAgPIE gdx file. Depending on the
#' product and the MAgPIE version (and factor cost realization), factor costs
#' are either already split into labor and capital, will be split in this
#' function, or are kept as the aggregate
#'
#' @export
#'
#' @param gdx GDX file
#' @param products products for which factor costs should be reported ("kcr",
#' "kli", "kres", "fish", or "pasture")
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
#' x <- factorCosts(gdx)
#' }
#'
factorCosts <- function(gdx, products = "kli", file = NULL, level = "regglo") {

  if (products == "pasture") {
    items <- "pasture"
    var <- "ov_cost_prod_past"
  }
  if (products == "fish") {
    items <- "fish"
    var <- "ov_cost_prod_fish"
  }
  if (products == "kli") {
    items <- findset("kli")
    var <- "ov_cost_prod_livst"
  }
  if (products == "kres") {
    items <- findset("kres")
    var <- "ov_cost_prod_kres"
  }
  if (products == "kcr") {
    items <- findset("kcr")
    var <- "ov_cost_prod_crop"
  }

  # old factor cost variable naming
  if (suppressWarnings(!is.null(readGDX(gdx, "ov_cost_prod")))) {
    factorCosts <- readGDX(gdx, "ov_cost_prod", react = "silent",
                          format = "first_found", select = list(type = "level"))[, , items]

    # for old sticky runs, we have shares to split between capital and labor costs
    capShareHist <- suppressWarnings(readGDX(gdx, "f38_historical_share",
                                                     react = "silent", format = "first_found"))
    if (!is.null(capShareHist)) {
      # special case for sticky dynamic, where crop factor costs are already split
      if (products == "kcr" && any(readGDX(gdx, "ov_cost_inv")[, , "level"] != 0)) {
        stop("Function does not work for crops with sticky dynamic results")
      }
      capShareReg <- readGDX(gdx, "f38_reg_parameters", react = "silent", format = "first_found")
      capShareHist <- readGDX(gdx, "f38_historical_share", react = "silent", format = "first_found")
      mapping <- readGDX(gdx, "i_to_iso", react = "silent", format = "first_found")
      gdpPPPpc <- toolAggregate(readGDX(gdx, "im_gdp_pc_ppp_iso", react = "silent", format = "first_found"),
                                  rel = mapping, from = "iso", to = "i", weight = NULL)

      shareCalibration <- capShareHist[, 2010, ] - (as.double(capShareReg[, , "slope"]) *
                              log10(gdpPPPpc[, 2010, ]) + as.double(capShareReg[, , "intercept"]))
      capitalShare <- capShareHist[, seq(1995, 2100, 5), ]
      capitalShare[, seq(2015, 2100, 5), ] <- shareCalibration + as.double(capShareReg[, , "intercept"]) +
        as.double(capShareReg[, , "slope"]) * log10(gdpPPPpc[, seq(2015, 2100, 5), ])

      factorCosts <- add_dimension(factorCosts, add = "cost_type", nm = "factor_costs")
      capitalCosts <- factorCosts * capitalShare[, getYears(factorCosts), ]
      laborCosts <- factorCosts - capitalCosts
      getNames(capitalCosts, dim = 1) <- "capital_costs"
      getNames(laborCosts, dim = 1) <- "labor_costs"

      factorCosts <- mbind(capitalCosts, laborCosts, factorCosts)
    }
  } else { # new factor cost variable naming
    factorCosts <- readGDX(gdx, var, react = "silent", format = "first_found", select = list(type = "level"))

    if (products %in% c("kres", "fish", "pasture")) {
      costShares <- readGDX(gdx, "p38_cost_share", react = "silent", format = "first_found")
      factorCosts <- costShares * factorCosts
    }

    factorCosts <- add_columns(factorCosts, addnm = "factor_costs", dim = 3.1)
    factorCosts[, , "factor_costs"] <- dimSums(factorCosts[, , "factor_costs", invert = TRUE], dim = 3.1)

    if (products == "kres") factorCosts <- dimSums(factorCosts, dim = 3.2)

    factorCosts <- add_dimension(factorCosts, dim = 3.2, add = "sector", nm = reportingnames(products))

    getNames(factorCosts, dim = 1) <- c("labor_costs", "capital_costs", "factor_costs")
  }

  factorCosts <- superAggregate(factorCosts, aggr_type = "sum", level = level)

  out(factorCosts, file)
}
