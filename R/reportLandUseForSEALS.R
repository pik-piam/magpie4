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

  if (!is.null(outFile)) {
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
    if (!file.exists(file.path(dir, magCellLand))) stop("Disaggregated land-use information not found. Run extra/disaggregation.R")

    magLand <- read.magpie(file.path(dir, magCellLand))
    magLand <- magLand[, selectyears, ]
  }

  write.magpie(magLand, file.path(dir, outFile), append = FALSE)

  # Report completion
  message(paste0("Finished writing SEALS land cover input into\n'", dir, "'"))
}
