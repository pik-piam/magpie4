#' @title NitrogenBudgetPasture
#' @description calculates projections of Nitrogen Budgets for Croplands from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param include_emissions TRUE also divides the N surplus into different emissions
#' @param level aggregation level, reg, glo or regglo, cell, grid, iso
#' @author Benjamin Leon Bodirsky, Edna J. Molina Bacca
#' @importFrom magpiesets findset
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom luscale superAggregate
#' @importFrom mstools toolFertilizerDistribution
#' @examples
#' \dontrun{
#' x <- NitrogenBudgetPasture(gdx)
#' }
#'
NitrogenBudgetPasture <- function(gdx, include_emissions = FALSE, level = "reg") {

  harvest    <- production(gdx, level = level, attributes = "nr", products = "pasture")
  fertilizer <- collapseNames(readGDX(gdx, "ov_nr_inorg_fert_reg", format = "first_found", select = list(type = "level"))[, , "past"])

  manure     <- dimSums(readGDX(gdx, "ov_manure", select = list(type = "level"))[, , "grazing"][, , "nr"], dim = c(3.2, 3.3))
  manure     <- gdxAggregate(gdx = gdx, weight = "ManureExcretion", x = manure, to = level, absolute = TRUE, products = readGDX(gdx, "kli"), awms = "grazing", agg = "awms")
  manure     <- dimSums(manure, dim = 3)

  dep   <- collapseNames(readGDX(gdx, "ov50_nr_deposition")[, , "past"][, , "level"])
  dep   <- gdxAggregate(gdx = gdx, weight = "land", x = dep, to = level, absolute = TRUE, types = "past")

  fix   <- land(gdx)[, , "past"] * readGDX(gdx, "f50_nr_fixation_rates_pasture")[, getYears(harvest), ]
  fix   <- gdxAggregate(gdx = gdx, weight = "production", x = fix, to = level, absolute = TRUE, products = "pasture", attributes = "nr")

  out <- mbind(
    setNames(harvest, "harvest"),
    setNames(manure, "grazing"),
    setNames(fix, "fixation_freeliving"),
    setNames(dep, "deposition")
  )

  if (level %in% c("cell", "grid", "iso")) {
    withdrawals <- out[, , "harvest"]
    organicinputs <- dimSums(out[, , c("grazing", "fixation_freeliving", "deposition")], dim = 3)

    NUE <- readGDX(gdx, "ov50_nr_eff_pasture", "ov_nr_eff_pasture", format = "first_found")[, , "level"]

    if (level == "cell") {
      mapping <- readGDX(gdx, "cell")
    } else if (level %in% c("grid","iso")) {
      clustermap_filepath <- Sys.glob(file.path(dirname(normalizePath(gdx)), "clustermap*.rds"))
      if(length(clustermap_filepath)==1) {
        mapping <- readRDS(clustermap_filepath)[, c("region", "cell")]
        names(mapping) <- c("i", "j")
      } else {
        stop("No mapping for toolFertilizerDistribution found")
      }
    }

    max_snupe <- 0.85

    fert <- toolFertilizerDistribution(iterMax = 50, maxSnupe = max_snupe,
                                       mapping = mapping, from = "j", to = "i", fertilizer = fertilizer, snupe = NUE,
                                       withdrawals = withdrawals, organicinputs = organicinputs)

  } else {
    fert <- gdxAggregate(x = fertilizer, gdx = gdx, to = level, absolute = TRUE)
  }

  out <- mbind(out, setNames(fert, "fertilizer"))

  # disaggregation can lead to inconsitent budgets, e.g. due to heterogeneity of deposition.
  # we correct this by adjusting harvest estimates, such that more is harvested in cells
  # where the budget allows for it.

  max_eff <- 0.95 # assuming a maximum plausible efficiency of pasture systems of 95 percent
  max_harvest <- dimSums(out[, , c("harvest"), invert = TRUE], dim = 3) * max_eff
  balanceflow <- out[, , c("harvest")] - max_harvest
  balanceflow[balanceflow < 0] <- 0 # this is the quantity where the surplus exceeds the inputs

  if (any(balanceflow > 0)) {
    if (level == "reg") {
      stop("negative values in the regional budgets should not occur.")
    }

    # distribute balanceflow as withdrawal proportional to surplus
    out[, , "harvest"] <- out[, , "harvest"] - balanceflow # we reduce the harvested quantities to avoid negative surplus

    surplus <- setNames(
      dimSums(out[, , c("harvest"), invert = TRUE], dim = 3) - dimSums(out[, , c("harvest")], dim = 3),
      "surplus"
    )

    clustermap_filepath <- readRDS(Sys.glob(file.path(dirname(normalizePath(gdx)), "clustermap*.rds")))
    for (region_x in unique(clustermap_filepath$region)) {
      cells <- clustermap_filepath$cell[which(clustermap_filepath$region == region_x)]

      message(paste0("balanceflow is a max of ",
                     round(max(dimSums(balanceflow[cells, , ], dim = 1) /
                                 dimSums(out[cells, , "harvest"], dim = 1)),
                           2) * 100,
                     " percent of harvest in region ", region_x, "\n"))

      redist_shr <- surplus[cells, , ] / dimSums(surplus[cells, , ], dim = 1)
      out[cells, , "harvest"] <- out[cells, , "harvest"] + redist_shr * dimSums(balanceflow[cells, , ], dim = 1)
    }
  }

  ### surplus and emissions
  out <- mbind(
    out,
    setNames(
      dimSums(out[, , c("harvest"), invert = TRUE], dim = 3) - dimSums(out[, , c("harvest")], dim = 3),
      "surplus"
    )
  )

  if (include_emissions) {
    emissions <- Emissions(gdx, type = c("n2o_n", "nh3_n", "no2_n", "no3_n"), level = "reg", unit = "element", subcategories = TRUE, lowpass = FALSE, inorg_fert_split = TRUE, cumulative = FALSE)
    types <- c("man_past")
    emissions <- emissions[, , types]
    emissions <- dimSums(emissions, dim = "emis_source")

    emissions <- gdxAggregate(gdx = gdx, x = emissions, weight = dimSums(out[, , "surplus"]), to = level, absolute = TRUE)

    out <- mbind(out, emissions)
  }


  ### error checks
  if (level == "reg") {

    out_surplus <- out[, , "surplus"]
    ov50_nr_surplus_pasture <- readGDX(gdx, "ov50_nr_surplus_pasture", format = "first_found", select = list(type = "level"))

    if (sum(abs(out_surplus - ov50_nr_surplus_pasture)) > 0.1) {
      warning("Surplus in gams and postprocessing dont match")
    }
    if (include_emissions) {
      out_emis <- dimSums(out[, , c("n2o_n", "nh3_n", "no2_n", "no3_n")], dim = 3)
      if (any((ov50_nr_surplus_pasture - out_emis) < 0)) {
        warning("Emissions exceed surplus. Maybe use rescale realization of 51_nitrogen")
      }
      if (any(((out_emis + 0.5 * 10^-10) / (out_surplus + 10^-10)) > 0.9)) {
        warning("N2 emissions in surplus very low")
      }
    }
  }

  return(out)
}
