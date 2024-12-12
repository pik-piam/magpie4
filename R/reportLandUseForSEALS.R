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
reportLandUseForSEALS <- function(magCellLand = "cell.land_0.5_share.mz", outFile = NULL,
                                  dir = ".", selectyears = c(2020, 2030, 2050)) {
  # -----------------------------------
  # Create NetCDF file for SEALS
  # -----------------------------------

  if (is.null(outFile)) {
    cfg <- gms::loadConfig(file.path(dir, "config.yml"))
    if (length(cfg) > 1) {
      title <- paste0("_", cfg$title)
    } else {
      title <- ""
    }
    outFile <- sub("\\.[^.]+$", paste0("_SEALS", title, ".nc"), magCellLand)
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
  lat <- ncdf4::ncdim_def("lat", "degrees_north", seq(89.75, -89.75, -0.5))
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

}
