#' @title reportFireEmissions.R
#' @description reads land cover data and deforestation data to produce future estimates of fire
#' emissions based on extrapolating GFED4.1s data from 2003-2016. Emissions are area x rho x emission factor,
#' where rho (inst/extdata/GFED_regional_dm_rho_scaled.csv) is the regional GFED dry-matter mass of a fire
#' type per m2 of the historical area of the class it is applied to; see the provenance file beside it.
#' The run's areas: forest (primforest + secdforest + forestry) for boreal, temperate, small deforestation and
#' peat fires; past + other for savanna and grassland fires; crop for agricultural fires (not reported); for
#' large deforestation fires, the natural-forest reduction without timber harvest (primary and secondary
#' forest reduction minus their harvested area, which regrows) plus the secondary-forest disturbance loss
#' (shifting agriculture), per year.
#' Fire CO2 is gross: deforestation and peat fire CO2 overlap with MAgPIE's land-use change CO2, and savanna
#' and forest fire CO2 is largely balanced by regrowth. Do not add it to land-use change CO2.
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global)
#' @return emissions as MAgPIE object (unit: Mt X/yr, plus cumulative Mt X/yr)
#' @author Michael Crawford
#' @importFrom dplyr %>%
#' @importFrom utils read.delim read.csv
#' @examples
#'   \dontrun{
#'     x <- reportFireEmissions(gdx, level = "glo")
#'   }
#'
#' @section Fire emission variables:
#' Reported for X = BC, CO, CO2, NO2, OC, SO2, N2O, CH4, NH3 and VOC. NO2 is NOx reported as mass of NO2
#' (GFED gives NOx as mass of NO; converted by 46/30).
#'
#' Name | Unit | Meta
#' ---|---|---
#' Emissions\|X\|AFOLU\|Land\|Fires | Mt X/yr | Total X emissions from fires
#' Emissions\|X\|AFOLU\|Land\|Fires\|+\|Forest Burning | Mt X/yr | X from forest fires
#' Emissions\|X\|AFOLU\|Land\|Fires\|Forest Burning\|+\|Boreal Forest | Mt X/yr | X from boreal
#' forest fires
#' Emissions\|X\|AFOLU\|Land\|Fires\|Forest Burning\|+\|Temperate Forest | Mt X/yr | X from temperate
#' forest fires
#' Emissions\|X\|AFOLU\|Land\|Fires\|Forest Burning\|+\|Tropical Forest | Mt X/yr | X from tropical
#' deforestation fires
#' Emissions\|X\|AFOLU\|Land\|Fires\|+\|Grassland Burning | Mt X/yr | X from grassland/savanna
#' fires
#' Emissions\|X\|AFOLU\|Land\|Fires\|+\|Peat Burning | Mt X/yr | X from peat fires
#' @md


reportFireEmissions <- function(gdx, level = "reg") {

  ###########################################################################
  # --- Process emission factors

  filePath <- system.file("extdata", "GFED_emissionFactors.txt", package = "magpie4")

  ef_raw <- utils::read.delim(
    file         = filePath,
    sep          = "",
    header       = FALSE,
    comment.char = "#",
    strip.white  = TRUE,
    stringsAsFactors = FALSE
  )

  ef <- ef_raw %>%
    dplyr::as_tibble() %>%
    stats::setNames(c("SPECIE", "SAVA", "BORF", "TEMF", "DEFO", "PEAT", "AGRI"))

  emissionFactorsSingleton <- c("BC", "CO", "CO2",  "NOx", "OC", "SO2", "N2O", "CH4", "NH3")
  emissionFactorsVOC    <- c(
    "NMHC",
    "C2H6", "CH3OH", "C2H5OH", "C3H8",
    "C2H2", "C2H4", "C3H6", "C5H8",
    "C10H16", "C7H8", "C6H6", "C8H10",
    "Toluene_lump", "Higher_Alkenes", "Higher_Alkanes",
    "CH2O", "C2H4O", "C3H6O",
    "HCN", "HCOOH", "CH3COOH", "MEK", "CH3COCHO", "HOCH2CHO"
  )

  vocTotals <- ef %>%
    dplyr::filter(.data$SPECIE %in% emissionFactorsVOC) %>%
    dplyr::summarize(
      SPECIE = "VOC",
      dplyr::across(c("SAVA", "BORF", "TEMF", "DEFO", "PEAT", "AGRI"),
                    ~sum(.x, na.rm = TRUE))
    )

  ef <- ef %>%
    dplyr::filter(.data$SPECIE %in% emissionFactorsSingleton) %>%
    dplyr::bind_rows(vocTotals)

  # GFED gives NOx as mass of NO; MAgPIE reports NOx as mass of NO2 (Emissions.R, no2_n x 46 / 14),
  # so the NOx factor is converted by M(NO2) / M(NO) = 46 / 30 before the rename to NO2 below
  stopifnot(sum(ef$SPECIE == "NOx") == 1)
  ef <- ef %>%
    dplyr::mutate(dplyr::across(c("SAVA", "BORF", "TEMF", "DEFO", "PEAT", "AGRI"),
                                ~ dplyr::if_else(.data$SPECIE == "NOx", .x * 46 / 30, .x)))

  # Add DEFO split columns (same emission factors for large and small fires)
  ef <- ef %>%
    dplyr::mutate(
      DEFO_largeFires = .data$DEFO,
      DEFO_smallFires = .data$DEFO
    ) %>%
    dplyr::select(!"DEFO")  # Remove the combined DEFO column

  ###########################################################################
  # --- Process GFED scaled dm measurements

  rho_scaled_path <- system.file("extdata", "GFED_regional_dm_rho_scaled.csv", package = "magpie4")
  rho_scaled <- utils::read.csv(file = rho_scaled_path, sep = ",", header = TRUE)

  ###########################################################################
  # --- Process areas (Mha/yr)

  # Read land stocks
  land <- land(gdx, level = "reg")

  h12 <- c("CAZ", "CHA", "EUR", "IND", "JPN", "LAM", "MEA", "NEU", "OAS", "REF", "SSA", "USA")
  regions <- getItems(land, dim = 1)
  if (!all(regions %in% h12)) {
    return("Currently only supported for H12 regions")
  }

  # Forest area (used for BORF, TEMF, PEAT, and DEFO_smallFires)
  area_forest <- dimSums(land[, , c("primforest", "secdforest", "forestry")], dim = 3) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(area = "area_forest")

  # Savannas
  area_sava <- dimSums(land[, , c("past", "other")], dim = 3) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(area = "area_sava")

  # Agricultural residue burning
  # This value may serve as a validation of our mechanistic version, but should not be used in MAgPIE reportings
  # as own-data -- we track it ourselves
  area_agri <- collapseDim(land[, , "crop"]) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(area = "area_agri")

  # Read deforestation fluxes for DEFO_largeFires: natural-forest reduction without timber harvest (harvested
  # area is part of the reduction and regrows as secondary forest, so it is not deforestation), plus the
  # secondary-forest disturbance loss (shifting agriculture)
  primforestReductionMha <- readGDX(gdx, "ov35_primforest_reduction", select = list(type = "level"))
  secdforestReductionMha <- dimSums(readGDX(gdx, "ov35_secdforest_reduction", select = list(type = "level")), dim = 3)
  primforestHarvestMha   <- readGDX(gdx, "ov35_hvarea_primforest", select = list(type = "level"))
  secdforestHarvestMha   <- dimSums(readGDX(gdx, "ov35_hvarea_secdforest", select = list(type = "level")), dim = 3)
  secdforestDegradMha    <- dimSums(readGDX(gdx, "p35_disturbance_loss_secdf", react = "silent"), dim = 3)

  area_defo <- primforestReductionMha - primforestHarvestMha + secdforestReductionMha - secdforestHarvestMha +
    secdforestDegradMha
  im_years <- collapseDim(m_yeardiff(gdx), dim = c(1, 3))
  area_defo <- area_defo / im_years

  # Sum the clusters to regions (all regions; the table gives 0 where there is no deforestation fire)
  area_defo <- dimSums(area_defo, dim = c(1.2, 3)) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(reg = "j") %>%
    dplyr::mutate(area = "area_defo")

  # Collect area data
  areas <- dplyr::bind_rows(area_forest, area_sava, area_agri, area_defo) %>%
    dplyr::mutate(value = ifelse(.data$t == 1995, NA, .data$value))

  ###########################################################################
  # --- Calculate emissions
  # Area (Mha) × Rho (kg/m²/yr) × EmissionFactor (g species/kg DM) = Mt species per year
  # Units: 10^10 m² × kg/m²/yr × 1e-3 = 10^7 kg/yr = 10 Mt/yr

  # Simplified mapping with single forest area and DEFO split
  area_mapping <- dplyr::tibble(
    area      = c("area_forest", "area_forest", "area_forest", "area_forest",
                  "area_sava", "area_agri", "area_defo"),
    fire_type = c("BORF", "TEMF", "DEFO_smallFires", "PEAT",
                  "SAVA", "AGRI", "DEFO_largeFires")
  )

  # Reshape rho_scaled to long format
  rho_long <- rho_scaled %>%
    tidyr::pivot_longer(cols = !"reg", names_to = "fire_type", values_to = "rho_value")

  # Reshape emission factors to long format
  ef_long <- ef %>%
    tidyr::pivot_longer(cols = !"SPECIE", names_to = "fire_type", values_to = "ef_value")

  # Join areas with fire types
  areas_expanded <- areas %>%
    dplyr::inner_join(area_mapping, by = "area", relationship = "many-to-many") %>%
    dplyr::rename(area_value = "value")

  # Calculate emissions: Area × Rho × EmissionFactor × 0.01
  # The 0.01 factor converts: Mha × kg/m²/yr × g/kg = 10^10 × 10^-3 × 10^-9 = 10^-2 Mt/yr
  emissions <- areas_expanded %>%
    dplyr::inner_join(rho_long, by = c("reg", "fire_type")) %>%
    dplyr::inner_join(ef_long, by = "fire_type", relationship = "many-to-many") %>%
    dplyr::mutate(emission = .data$area_value * .data$rho_value * .data$ef_value * 0.01) %>%
    dplyr::select("reg", "t", "fire_type", "SPECIE", "emission")

  # Rename NOx to NO2
  emissions <- emissions %>%
    dplyr::mutate(SPECIE = ifelse(.data$SPECIE == "NOx", "NO2", .data$SPECIE))

  ###########################################################################
  # --- Compose reporting variables

  # Function to create report for one emission species
  create_species_report <- function(emissions, species) {
    # Filter for current species
    emissions_species <- emissions %>%
      dplyr::filter(.data$SPECIE == species)

    # 1. Total Fires (now includes both DEFO components)
    fires_total <- emissions_species %>%
      dplyr::filter(.data$fire_type %in% c("BORF", "TEMF", "DEFO_largeFires", "DEFO_smallFires", "SAVA", "PEAT")) %>%
      dplyr::group_by(.data$reg, .data$t) %>%
      dplyr::summarise(emission = sum(.data$emission, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(variable = paste0("Emissions|", species, "|AFOLU|Land|Fires"))

    # 2. Forest Burning (sum of all forest-related fires)
    forest_burning <- emissions_species %>%
      dplyr::filter(.data$fire_type %in% c("BORF", "TEMF", "DEFO_largeFires", "DEFO_smallFires")) %>%
      dplyr::group_by(.data$reg, .data$t) %>%
      dplyr::summarise(emission = sum(.data$emission, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(variable = paste0("Emissions|", species, "|AFOLU|Land|Fires|+|Forest Burning"))

    # 3-5. Forest Burning subcategories
    boreal_forest <- emissions_species %>%
      dplyr::filter(.data$fire_type == "BORF") %>%
      dplyr::mutate(variable = paste0("Emissions|", species, "|AFOLU|Land|Fires|Forest Burning|+|Boreal Forest"))

    temperate_forest <- emissions_species %>%
      dplyr::filter(.data$fire_type == "TEMF") %>%
      dplyr::mutate(variable = paste0("Emissions|", species, "|AFOLU|Land|Fires|Forest Burning|+|Temperate Forest"))

    # Tropical forest: sum of large and small DEFO fires
    tropical_forest <- emissions_species %>%
      dplyr::filter(.data$fire_type %in% c("DEFO_largeFires", "DEFO_smallFires")) %>%
      dplyr::group_by(.data$reg, .data$t) %>%
      dplyr::summarise(emission = sum(.data$emission, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(variable = paste0("Emissions|", species, "|AFOLU|Land|Fires|Forest Burning|+|Tropical Forest"))

    # 6. Grassland Burning
    grassland_burning <- emissions_species %>%
      dplyr::filter(.data$fire_type == "SAVA") %>%
      dplyr::mutate(variable = paste0("Emissions|", species, "|AFOLU|Land|Fires|+|Grassland Burning"))

    # 7. Peat Burning
    peat_burning <- emissions_species %>%
      dplyr::filter(.data$fire_type == "PEAT") %>%
      dplyr::mutate(variable = paste0("Emissions|", species, "|AFOLU|Land|Fires|+|Peat Burning"))

    # Combine in desired order
    report_species <- dplyr::bind_rows(
      fires_total,
      forest_burning,
      boreal_forest,
      temperate_forest,
      tropical_forest,
      grassland_burning,
      peat_burning
    ) %>%
      dplyr::select("reg", "t", "variable", "emission")

    # Convert to wide format and magclass
    report_wide <- report_species %>%
      tidyr::pivot_wider(names_from = "variable", values_from = "emission") %>%
      as.magpie(spatial = 1, temporal = 2)

    # Add units
    getNames(report_wide) <- paste0(getNames(report_wide), " (Mt ", species, "/yr)")

    return(report_wide)
  }

  # Main processing - get unique species and process each
  unique_species <- unique(emissions$SPECIE)
  reportAnnual <- NULL

  for (species in unique_species) {
    species_report <- create_species_report(emissions, species)

    if (is.null(reportAnnual)) {
      reportAnnual <- species_report
    } else {
      reportAnnual <- mbind(reportAnnual, species_report)
    }
  }

  ###########################################################################
  # --- Calculate cumulative emissions

  # cumulative report in Mt X
  cumYears <- collapseDim(m_yeardiff(gdx))
  reportCum <- reportAnnual
  reportCum[, "y1995", ] <- 0
  reportCum <- reportCum * cumYears[, getYears(reportCum), ]
  reportCum <- as.magpie(apply(reportCum, c(1, 3), cumsum))

  # insert "Cumulative" and drop "/yr" from the unit
  newNames <- getNames(reportCum)
  newNames <- sub("\\|Land\\|", "|Land|Cumulative|", newNames)
  newNames <- sub("/yr\\)", ")", newNames)
  reportCum <- setNames(reportCum, newNames)

  # Combine annual and cumulative reports
  report <- mbind(reportAnnual, reportCum)

  ###########################################################################
  # --- Format results for MAgPIE reporting
  # Aggregate to requested level

  if (level != "reg") {
    report <- superAggregateX(report, aggr_type = "sum", level = level)
  }

  return(report)
}