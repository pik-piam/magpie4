#' @title reportLandUseForSEALS
#' @description Writes MAgPIE land use projections to a specific NetCDF
#' that can be read by the Spatial Economic Allocation Landscape Simulator (SEALS) model
#' for generating high resolution land use maps.
#'
#' @export
#'
#' @param magCellLand Disaggregated land use (grid-cell land area share) as
#' magclass object or file (.mz) from a MAgPIE run.
#' @param outFile a file name the output should be written to using \code{ncdf4::nc_create} and \code{ncdf4::ncvar_put}
#' @param dir output directory which contains cellular magpie output
#' @param scenName Optional scenario name
#' @param selectyears Numeric vector of years to provide data for.
#'
#' @return Proportions of different land use classes per grid sell in a NetCDF format.
#'
#' @author Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- reportLandUseForSEALS(
#'   magCellLand = "cell.land_0.5_share.mz",
#'   outFile = "cell.land_0.5_SEALS.nc",
#'   selectyears = c(2020, 2030, 2050)
#' )
#' }
#'
reportLandUseForSEALS <- function(magCellLand = "cell.land_0.5_share.mz", outFile = "cell.land_0.5_SEALS.nc",
                                  scenName = NULL, dir = ".", selectyears = c(2020, 2030, 2050)) {
  # -----------------------------------
  # Create NetCDF file for SEALS
  # -----------------------------------

  if (!is.null(scenName)) {
    outFile <- sub(".nc", paste0("_", scenName, ".nc"), outFile)
  }

  ### Open the MAgPIE cell output
  if (is.magpie(magCellLand)) {
    magLand <- magCellLand[, selectyears, ]
  } else {
    if (!file.exists(file.path(dir, magCellLand))) stop("Disaggregated land-use information not found")

    magLand <- read.magpie(file.path(dir, magCellLand))
    magLand <- magLand[, selectyears, ]
  }

  ### Define dimensions
  lon <- ncdf4::ncdim_def("lon", "degrees_east", seq(-179.75, 179.75, 0.5))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", seq(-89.75, 89.75, 0.5))
  time <- ncdf4::ncdim_def("time", "years", selectyears, unlim = TRUE)

  # Create a new netCDF file with all variable names
  sealsLand <- ncdf4::nc_create(file.path(dir, outFile),
    vars = list(
      ncdf4::ncvar_def("crop", "grid-cell land area fraction", dim = list(lon, lat, time), missval = -9999),
      ncdf4::ncvar_def("past", "grid-cell land area fraction", dim = list(lon, lat, time), missval = -9999),
      ncdf4::ncvar_def("primforest", "grid-cell land area fraction", dim = list(lon, lat, time), missval = -9999),
      ncdf4::ncvar_def("secdforest", "grid-cell land area fraction", dim = list(lon, lat, time), missval = -9999),
      ncdf4::ncvar_def("forestry", "grid-cell land area fraction", dim = list(lon, lat, time), missval = -9999),
      ncdf4::ncvar_def("urban", "grid-cell land area fraction", dim = list(lon, lat, time), missval = -9999),
      ncdf4::ncvar_def("other", "grid-cell land area fraction", dim = list(lon, lat, time), missval = -9999)
    )
  )

  # Set variable names
  vnames <- c("crop", "past", "primforest", "secdforest", "forestry", "urban", "other")

  # Write values to NetCDF
  for (vname in vnames) {
    r <- as.SpatRaster(magLand[, , vname])
    r <- terra::extend(r, terra::ext(-180, 180, -90, 90))
    vals <- as.vector(terra::values(r))
    ncdf4::ncvar_put(sealsLand, vname, vals)
  }

  # Close connection
  ncdf4::nc_close(sealsLand)

  # Report completion
  message(paste0("Finished writing '", outFile, "' into\n'", dir, "'"))


  # -------------------------------------------
  # Create SEALS scenario definitions CSV
  # -------------------------------------------

  message("Creating SEALS scenario definitions CSV")

  cfg <- gms::loadConfig(file.path(dir, "config.yml"))
  title <- cfg$title

  rcp <- unlist(strsplit(cfg$input["cellular"], "_"))[6]
  rcp <- paste0("rcp", substr(rcp, nchar(rcp) - 1, nchar(rcp)))

  ssp <- tolower(cfg$gms$c09_pop_scenario)

  if (length(cfg$seals_years) != 0) {
    sealsYears <- cfg$seals_years[cfg$seals_years > 2020]
    sealsYears <- paste(sealsYears, collapse = " ")
  } else {
    sealsYears <- "2050"
  }

  scenarioType <- ifelse(grepl("default|bau|ssp\\d-ref", tolower(title)), "bau", "policy")

  if (cfg$gms$c22_protect_scenario == "none") {
    consv <- cfg$gms$c22_base_protect
  } else {
    consv <- cfg$gms$c22_protect_scenario
  }

  sealsConfig <- c(
    "input/seals_scenario_config.csv",
    "../input/seals_scenario_config.csv",
    "../../input/seals_scenario_config.csv"
  )
  sealsConfig <- suppressWarnings(sealsConfig[min(which(file.exists(sealsConfig)))])
  if (!is.na(sealsConfig)) {
    sealsConfig <- read.csv(sealsConfig)
    sealsConfig[nrow(sealsConfig), "scenario_label"] <- title
    sealsConfig[nrow(sealsConfig), "scenario_type"] <- scenarioType
    sealsConfig[nrow(sealsConfig), "exogenous_label"] <- ssp
    sealsConfig[nrow(sealsConfig), "climate_label"] <- rcp
    sealsConfig[nrow(sealsConfig), "counterfactual_label"] <- title
    sealsConfig[nrow(sealsConfig), "comparison_counterfactual_labels"] <- ifelse(scenarioType == "bau", "", "bau")
    sealsConfig[, "coarse_projections_input_path"] <- file.path(dir, outFile)
    sealsConfig[nrow(sealsConfig), "years"] <- sealsYears
    sealsConfig[nrow(sealsConfig), "calibration_parameters_source"] <- sub(
      "WDPA", consv, sealsConfig[nrow(sealsConfig), "calibration_parameters_source"]
    )

    write.csv(sealsConfig, file.path(dir, paste0("seals_scenario_config_", title, ".csv")),
      row.names = FALSE, na = ""
    )
  } else {
    stop("Could not find seals_scenario_config.csv file template")
  }

  message("Finished writing SEALS scenario definitions CSV")

}
