#' Guard against a livestock distribution weight normalised within country
#'
#' Disaggregating regional values to the grid with gdxAggregate() renormalises the weight
#' within each region. A weight normalised within country agrees with that only if a region
#' holds a single country, which is false for most MAgPIE regions, and would silently
#' misallocate between the countries of a region. This checks for that signature - most
#' countries' cells summing to ~1 - and stops.
#' @author Bin Lin
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
