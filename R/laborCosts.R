#' @title laborCosts
#' @description reads labor costs for crop and livestock production from gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param products products for which labor costs should be reported ("kcr" or "kli",
#' for other products use factorCosts())
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("grid" or "iso", for regional/global
#' use factorCosts())
#' @param dir for gridded outputs: magpie output directory which contains
#' a mapping file (rds) for disaggregation
#' @return MAgPIE object containing labor costs [million US$17]
#' @author Debbora Leip
#' @importFrom magclass add_dimension getNames mbind
#' @importFrom luscale superAggregate
#' @importFrom madrat toolAggregate
#' @examples
#' \dontrun{
#' x <- laborCosts(gdx)
#' }
#'
laborCosts <- function(gdx, products = "kcr", file = NULL, level = "grid", dir = ".") {

  if (!(level %in% c("iso", "grid"))) {
    stop("This function is only for iso and grid level. For regional/global results use the function factorCosts()")
  }

  prod <- production(gdx, level = level, products = products, dir = dir)
  if (level == "iso") prod <- toolCountryFill(prod, fill = 0)

  if (suppressWarnings(!is.null(readGDX(gdx, "i38_fac_req")))) { # new factor requirements implementation
    if (products == "kcr") {
      factorRequirements <- readGDX(gdx, "i38_fac_req", react = "silent", format = "first_found")
      costShare <- readGDX(gdx, c("pm_factor_cost_shares", "pm_cost_share_crops", "p38_cost_share"),
                           react = "silent", format = "first_found")
      years <- intersect(getYears(factorRequirements), getYears(costShare))
      costsPerOutput <- factorRequirements[, years, ] * costShare[, years, "labor", drop = TRUE]
    } else if (products == "kli") {
      regression <- readGDX(gdx, "i70_cost_regr", react = "silent", format = "first_found")[, , "fish", invert = TRUE]
      sysToKli <- readGDX(gdx, "sys_to_kli", react = "silent", format = "first_found")
      productivity <- toolAggregate(readGDX(gdx, "i70_livestock_productivity", react = "silent",
                                            format = "first_found"), from = "sys", to = "kli", rel = sysToKli, dim = 3)
      costShare <- readGDX(gdx, c("pm_factor_cost_shares", "p70_cost_share_livst"),
                           react = "silent", format = "first_found")
      years <- intersect(getYears(productivity), getYears(costShare))
      costsPerOutput <- (regression[, , "cost_regr_a", drop = TRUE] + regression[, , "cost_regr_b", drop = TRUE] *
                           productivity[, years, ]) * costShare[, years, "labor", drop = TRUE]
    } else {
      stop("This function only calculates labor costs for crops and livstock. For other products use factorCosts()")
    }

    costsPerOutput <- gdxAggregate(gdx, costsPerOutput, to = level, absolute = FALSE, dir = dir)
  } else {
    costsPerOutput <- NULL
  }

  x <- prod * costsPerOutput

  out(x, file)
}
