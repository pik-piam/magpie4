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
readLivestockDistribution <- function(gdx, products, years) {
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

#' Guard against a livestock distribution weight normalised within country
#'
#' Disaggregating regional values to the grid with gdxAggregate() renormalises the weight
#' within each region. A weight normalised within country agrees with that only if a region
#' holds a single country, which is false for most MAgPIE regions, and would silently
#' misallocate between the countries of a region. This checks for that signature - most
#' countries' cells summing to ~1 - and stops.
#' @noRd
checkExtensiveLivestockDist <- function(dist, distFile) {
  if (!"iso" %in% getSets(dist)) {
    return(invisible(NULL))
  }
  iso <- getItems(dist, dim = "iso", full = TRUE)
  if (is.null(iso)) {
    return(invisible(NULL))
  }
  isoSum <- tapply(as.numeric(dist[, 1, 1]), factor(iso), sum, na.rm = TRUE)
  isoSum <- isoSum[is.finite(isoSum) & isoSum > 0]
  if (length(isoSum) < 10) {
    return(invisible(NULL))
  }
  nUnit <- sum(abs(isoSum - 1) < 1e-6)
  if (nUnit / length(isoSum) > 0.9) {
    stop(basename(distFile), " is normalised within country (", nUnit, " of ",
         length(isoSum), " countries sum to 1), but cluster-to-grid disaggregation ",
         "needs an extensive weight. Regenerate the input with ",
         "calcOutput(\"LivestockDistribution\", output = \"head\", ...) in fullCELLULARMAGPIE.")
  }
  invisible(NULL)
}
