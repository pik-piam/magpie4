#' @title reportLandUseForSEALS
#' @description Restructures NetCDF of magpie land use output and writes it to a specific NetCDF
#' that can be read by the Spatial Economic Allocation Landscape Simulator (SEALS) model
#' for generating high resolution land use maps.
#'
#' @export
#'
#' @param magCellLand Disaggregated land use (grid-cell land area share) in NetCDF format from a MAgPIE run.
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
#' x <- reportLandUseForSEALS(magCellLand = "cell.land_0.5_share.nc",
#'                            outFile = "cell.land_0.5_SEALS.nc",
#'                            selectyears = c(2015, 2050))
#' }
#'
reportLandUseForSEALS <- function(magCellLand = "cell.land_0.5_share.nc", outFile = "cell.land_0.5_SEALS.nc",
                                  scenName = NULL, dir = ".", selectyears = c(2015, 2050)) {
  if (!is.null(scenName)) {
    outFile <- sub(".nc", paste0("_", scenName, ".nc"), outFile)
  }

  ### Open the MAgPIE output NetCDF
  magLand <- ncdf4::nc_open(file.path(dir, magCellLand))

  ### Extract dimensions
  lon <- ncdf4::ncdim_def("lon", "degrees_east", ncdf4::ncvar_get(magLand, names(magLand$dim)[1]))
  # Make sure that order of latitudinal coordinates is in the right order
  # Check whether latitudinal coordinates are ascending or descending
  latAscends <- all(diff(ncdf4::ncvar_get(magLand, "latitude")) > 0)
  if (latAscends) {
    lat <- ncdf4::ncdim_def("lat", "degrees_north", rev(ncdf4::ncvar_get(magLand, names(magLand$dim)[2])))
  } else {
    lat <- ncdf4::ncdim_def("lat", "degrees_north", ncdf4::ncvar_get(magLand, names(magLand$dim)[2]))
  }
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
    yrIndx <- which(ncdf4::ncvar_get(magLand, names(magLand$dim)[3]) %in% selectyears)
    if (latAscends) {
      ncdf4::ncvar_put(sealsLand, vname, ncdf4::ncvar_get(magLand, vname)[, lat$len:1, yrIndx])
    } else {
      ncdf4::ncvar_put(sealsLand, vname, ncdf4::ncvar_get(magLand, vname)[, , yrIndx])
    }
  }

  # Close connection
  ncdf4::nc_close(sealsLand)

  # Report completion
  message(paste0("Finished writing '", outFile, "' into\n'", dir, "'"))
}
