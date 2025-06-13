#' @title reportExtraResidueEmissions.R
#' @description reads residue biomass from a MAgPIE gdx file and multiplies DM by emission factors from GFED
#'
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global)
#' @return emissions as MAgPIE object (unit: Mt X/yr, plus cumulative Mt X/yr)
#' @author Michael Crawford
#' @examples
#'   \dontrun{
#'     x <- reportExtraResidueEmissions(gdx, level = "glo")
#'   }

reportExtraResidueEmissions <- function(gdx, level = "reg") {

  filePath <- system.file(
    "extdata",
    "GFED_emissionFactors.txt",
    package = "magpie4"
  )

  raw <- utils::read.delim(
    file         = filePath,
    sep          = "",
    header       = FALSE,
    comment.char = "#",
    strip.white  = TRUE,
    stringsAsFactors = FALSE
  )

  emissionFactors <- raw |>
    dplyr::as_tibble() |>
    setNames(c("SPECIE", "SAVA", "BORF", "TEMF", "DEFO", "PEAT", "AGRI")) |>
    as.magpie()

  # select only agricultural waste burning emission factors
  emissionFactors <- collapseDim(emissionFactors[, , "AGRI"])

  # convert from g/kg to Mt per Mt DM
  emissionFactors <- emissionFactors * 1e-3

  emissionFactorsSingleton <- c("BC", "CO", "CO2", "OC", "SO2")
  emissionFactorsVOC    <- c(
    "NMHC",
    "C2H6", "CH3OH", "C2H5OH", "C3H8",
    "C2H2", "C2H4", "C3H6", "C5H8",
    "C10H16", "C7H8", "C6H6", "C8H10",
    "Toluene_lump", "Higher_Alkenes", "Higher_Alkanes",
    "CH2O", "C2H4O", "C3H6O",
    "HCN", "HCOOH", "CH3COOH", "MEK", "CH3COCHO", "HOCH2CHO"
  )

  # annual report in Mt X / yr
  residues  <- collapseDim(gdx2::readGDX(gdx, "ov_res_ag_burn", react  = "silent", format = "first_found")[, , "level"])
  residueDM <- collapseDim(dimSums(residues[, , "dm"], dim = 3.1)) # sum over crop types

  emissions       <- residueDM * emissionFactors
  emissionsSingle <- emissions[, , emissionFactorsSingleton]
  emissionsVOC    <- dimSums(emissions[, , emissionFactorsVOC], dim = 3)
  getNames(emissionsVOC) <- "VOC"

  emissions <- mbind(
    emissionsSingle,
    emissionsVOC
  )

  speciesNames <- getItems(emissions, dim = 3.1)
  reportList <- lapply(speciesNames, function(sp) {
    slice <- emissions[, , sp]
    name  <- paste0("Emissions|", sp, "|Land|Biomass Burning|+|Burning of Crop Residues (Mt ", sp, "/yr)")
    setNames(slice, name)
  })
  reportAnnual <- do.call(mbind, reportList)

  # cumulative report in Mt X
  cumYears    <- collapseDim(m_yeardiff(gdx))
  reportCum   <- reportAnnual
  reportCum[, "y1995", ] <- 0
  reportCum   <- reportCum * cumYears[, getYears(reportCum), ]
  reportCum   <- as.magpie(apply(reportCum, c(1, 3), cumsum))

  # insert “Cumulative” and drop “/yr” from the unit
  newNames  <- getNames(reportCum)
  newNames  <- sub("\\|Land\\|", "|Land|Cumulative|", newNames)
  newNames  <- sub("/yr\\)",     ")",                 newNames)
  reportCum <- setNames(reportCum, newNames)

  report <- mbind(reportAnnual, reportCum)

  if (level != "reg") {
    report <- superAggregateX(report, aggr_type = "sum", level = level)
  }

  return(report)
}
