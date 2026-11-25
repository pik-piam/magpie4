#' @title reportBiochar
#' @description reports biochar production, applied and produced CO2 removals,
#'   cumulative CO2 removals, and soil stock of biochar
#'
#' @export
#'
#' @param gdx GDX file
#' @param level The aggregation level to be used ("regglo" by default)
#'
#' @return Biochar production, applied and produced CO2 removals, cumulative CO2
#'   removals, and soil stock as MAgPIE object
#' @author Kristine Karstens, Isabelle Weindl
#' @importFrom gdx2 readGDX
#' @examples
#'
#'   \dontrun{
#'     x <- reportBiochar(gdx)
#'   }

reportBiochar <- function(gdx, level = "regglo") {

  ### report production values
  biocharProduction  <- biochar(gdx, indicator = "bc_production", level = level, feedstockAggr = TRUE,
                                systemAggr = FALSE, attributes = c("ge", "dm", "c"))

  if (is.null(biocharProduction)) {
    return(NULL)
  }

  biocharProduction  <- dimOrder(biocharProduction, perm = c(2, 1), dim = 3)
  getNames(biocharProduction) <- paste0("Production|Biochar.", getNames(biocharProduction))

  out <- mbind(setNames(biocharProduction[, , "ge"] / 1000,
                        gsub("\\.ge\\.", "\\|Energy content\\|",
                             paste0(getNames(biocharProduction[, , "ge"]), " (EJ/yr)"))),
               setNames(dimSums(biocharProduction[, , "ge"], dim = 3.3) / 1000, "Production|Biochar|Energy content (EJ/yr)"),
               setNames(biocharProduction[, , "dm"],
                        gsub("\\.dm\\.", "\\|Dry Matter\\|",
                             paste0(getNames(biocharProduction[, , "dm"]), " (Mt DM/yr)"))),
               setNames(dimSums(biocharProduction[, , "dm"], dim = 3.3), "Production|Biochar|Dry Matter (Mt DM/yr)"),
               setNames(biocharProduction[, , "c"],
                        gsub("\\.c\\.", "\\|Carbon\\|",
                             paste0(getNames(biocharProduction[, , "c"]), " (Mt C/yr)"))),
               setNames(dimSums(biocharProduction[, , "c"], dim = 3.3), "Production|Biochar|Carbon (Mt C/yr)"))

  out <- magpiesets::summationhelper(round(out, 8), sep = "++", excludeLevels = 3)

  ### report CDR
  biocharCDRApplied <- biochar(gdx, indicator = "bc_stable_carbon", level = level, feedstockAggr = TRUE,
                               systemAggr = FALSE, attributes = "c") * 44 / 12

  biocharProductionC <- biochar(gdx, indicator = "bc_production", level = level, feedstockAggr = TRUE,
                                systemAggr = TRUE, attributes = "c")
  s63_BC100 <- readGDX(gdx, "s63_BC100")
  biocharCDRProduced <- setNames(s63_BC100, "c") * biocharProductionC * 44 / 12

  ### Cumulative Emissions
  timesteps <- m_yeardiff(gdx)

  cumulativeCDR <- function(cdr) {
    cdrCum <- cdr * timesteps[, getYears(cdr), ]
    values <- aperm(apply(as.array(cdrCum), c(1, 3), cumsum), c(2, 1, 3))
    dimnames(values) <- list(getCells(cdrCum), getYears(cdrCum), getNames(cdrCum))
    cdrCum <- new.magpie(getCells(cdrCum), getYears(cdrCum), getNames(cdrCum), fill = 0)
    dimnames(cdrCum) <- dimnames(values)
    cdrCum[] <- values
    cdrCum <- cdrCum - setYears(cdrCum[, 1995, ], NULL)
    cdrCum <- cdrCum / 1000

    return(cdrCum)
  }

  biocharCDRAppliedCum <- cumulativeCDR(biocharCDRApplied)
  biocharCDRProducedCum <- cumulativeCDR(biocharCDRProduced)

  emissions <- mbind(
    setNames(-biocharCDRApplied, "Emissions|CO2|Land|Land-use Change|Biochar|Applied (Mt CO2/yr)"),
    setNames(-biocharCDRProduced, "Emissions|CO2|Land|Land-use Change|Biochar|Produced (Mt CO2/yr)"),
    setNames(-biocharCDRAppliedCum, "Emissions|CO2|Land|Cumulative|Land-use Change|Biochar|Applied (Gt CO2)"),
    setNames(-biocharCDRProducedCum, "Emissions|CO2|Land|Cumulative|Land-use Change|Biochar|Produced (Gt CO2)")
  )

  out <- mbind(out, emissions)

  ### biochar stock in cropland soils
  bcSoilStock <- biochar(gdx, indicator = "bc_soil_stock", level = level)
  out <- mbind(out, setNames(bcSoilStock, "Resources|Biochar|Stock in Cropland (t DM/ha)"))

  return(out)
}
