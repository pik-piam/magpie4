#' @title kayaFractions
#' @description Calculates the elements t, d1, d2/d1, ..., dn/dn-1, t/dn in a kaya-like identity of the form
#' t = d1 * d2/d1 * ... * dn/dn-1 * t/dn, based on the variables t, d1, d2, ..., dn.
#'
#' @export
#'
#' @param data MAgPIE object with target variable and variables to calculate drivers. Needs to have the target variable
#' as first column in the data dimension, and the variables to calculate the drivers in the following columns. I.e.
#' getItems(data, dim = 3) should return c(t, d1, d2, ..., dn). If decomposition is to be calculated for multiple
#' scenarios, the data object can have a scenario dimension in 3.2.
#' @param driverNames Names of the drivers in the data object. If NULL, names of the drivers are set based on the names
#' of the variables d1, d2, ..., dn in the data object (i.e. "d2/d1", "d3/d2", ..., "t/dn"). Name of the target variable
#' is always kept as provided in the data object. Default is NULL.
#' @param fixTimeSteps Logical. For a consistent decomposition, the time steps in the data object need to be of equal
#' length. If fixTimeSteps is TRUE, the function will check if the time steps are of equal length and if not, will
#' interpolate the data linearly to have equal time steps. If fixTimeSteps is FALSE, the function will only throw a
#' warning if the time steps are not of equal length. Default is TRUE.
#'
#' @return The function returns a magpie object containing the target variable t and the drivers of the target variable
#' @seealso \code{\link{kayaLaspeyres}}
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' data <- new.magpie(cells_and_regions = c("EUR", "SSA", "USA", "LAM", "IND", "OAS"),
#'                    years = c(2000, 2005, 2010),
#'                    names = as.vector(outer(c("Area", "Population", "Supply"),
#'                                            c("SSP1", "SSP2"), paste, sep = ".")),
#'                    sets = c("Region", "Year", "Variable", "Scenario"), fill = runif(108))
#' kayaFractions(data)
#' }
#'
kayaFractions <- function(data, driverNames = NULL, fixTimeSteps = FALSE) {
  # check whether decomposition is allowed
  years <- getYears(data, as.integer = TRUE)
  intervals <- unique(years[2:length(years)] - years[1:(length(years) - 1)])
  if (length(intervals) > 1) {
    if (isFALSE(fixTimeSteps)) {
      warning(paste("Timestep length is not constant! Decomposition only makes sense for constant timesteps.",
                    "Please consider setting `fixTimeSteps = TRUE`"))
    } else if (isTRUE(fixTimeSteps)) {
      if (!requireNamespace("FRACTION", quietly = TRUE)) stop("Package 'FRACTION' needed to fix timestep lengths.")
      warning(paste("Timestep length is not constant! Missing years are filled by",
                    "linear interpolation for each variable."))
      timeStep <- intervals[1]
      for (i in intervals[2:length(intervals)]) {
        timeStep <- FRACTION::gcd(timeStep, i)
      }
      data <- time_interpolate(data, setdiff(seq(min(years), max(years), timeStep), years),
                               integrate_interpolated_years = TRUE)
    }
  }

  out <- add_columns(data, dim = 3.1, addnm = "lastFraction")
  names <- getNames(data, dim = 1)

  for (i in 3:length(names)) out[, , names[i]] <- data[, , names[i]] / data[, , names[i - 1]]

  out[, , "lastFraction"] <- data[, , names[1], drop = TRUE] / data[, , names[length(names)], drop = TRUE]

  if (is.null(driverNames)) {
    driverNames <- names
    for (i in 3:length(names)) driverNames[i] <- paste(names[i], names[i - 1], sep = "/")
    driverNames <- c(driverNames, paste(names[1], names[length(names)], sep = "/"))
  } else {
    driverNames <- c(names[1], driverNames)
  }

  getNames(out, dim = 1) <- driverNames

  return(out[, , , drop = TRUE])
}
