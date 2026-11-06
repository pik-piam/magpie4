#' Read the gridded livestock distribution next to the gdx
#'
#' Reads f71_livestock_distribution_0.5.mz from the folder of the gdx, checks that it holds
#' livestock heads and not shares normalised within country, and holds it constant beyond
#' its last year.
#' @param gdx GDX file
#' @param products livestock products to read
#' @param years years the distribution is needed for
#' @return MAgPIE object with livestock heads per grid cell
#' @author Bin Lin
#' @importFrom magclass read.magpie time_interpolate
#' @noRd
livestockDistribution <- function(gdx, products, years) {
  lvstDistFile <- file.path(dirname(normalizePath(gdx)), "f71_livestock_distribution_0.5.mz")
  if (!file.exists(lvstDistFile)) {
    stop("disaggLvst = 'glw' requested but distribution file not found: ", lvstDistFile)
  }
  lvstDist <- read.magpie(lvstDistFile)[, , products]
  checkExtensiveLivestockDist(lvstDist, lvstDistFile)
  lvstDist <- time_interpolate(lvstDist, interpolated_year = years,
                               integrate_interpolated_years = FALSE, extrapolation_type = "constant")
  return(lvstDist)
}
