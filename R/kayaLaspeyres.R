#' @title kayaLaspeyres
#' @description Calculates the Laspeyres decomposition of a changes in a target variable t into drivers based on a
#' kaya-like identity. The kaya-like identity needs to be of the form t = d1 * d2/d1 * d3/d2 * ... * dn/dn-1 * t/dn,
#' where d1, d2/d1, ..., dn/dn-1, t/dn are the drivers of change in the target variable t. The Laspeyres decomposition
#' calculates an additive decomposition of the change over time in the target variable based on the changes in the
#' drivers (for formula of the laspeyres decomposition see comment in .laspeyresDriver). The function returns the
#' change in the target variable and the decomposition of the change into the drivers.
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
#' @param type Type of the output. "relative" returns the relative change in the target variable from each time step to
#' the next and its decomposition by drivers, "absolute" returns the absolute change over time and its decomposition.
#' Default is "absolute".
#' @param fixTimeSteps Logical. For a consistent decomposition, the time steps in the data object need to be of equal
#' length. If fixTimeSteps is TRUE, the function will check if the time steps are of equal length and if not, will
#' interpolate the data linearily to have equal time steps. If fixTimeSteps is FALSE, the function will only throw a
#' warning if the time steps are not of equal length. Default is TRUE.
#'
#' @return The function returns the change in the target variable and the decomposition of the change into the drivers
#' either in percentage points or absolute values (unit of target variable).
#' @seealso \code{\link{kayaFractions}}
#' @author Debbora Leip
#' @importFrom utils combn
#' @examples
#' \dontrun{
#' data <- new.magpie(cells_and_regions = c("EUR", "SSA", "USA", "LAM", "IND", "OAS"),
#'                    years = c(2000, 2005, 2010),
#'                    names = as.vector(outer(c("Area", "Population", "Supply"),
#'                                            c("SSP1", "SSP2"), paste, sep = ".")),
#'                    sets = c("Region", "Year", "Variable", "Scenario"), fill = runif(108))
#' kayaLaspeyres(data)
#' }
#'
kayaLaspeyres <- function(data, driverNames = NULL, type = "relative", fixTimeSteps = TRUE) {# nolint: cyclocomp_linter

  # create factors from original variables
  data  <- kayaFractions(data, driverNames, fixTimeSteps = fixTimeSteps)

  # if only one scenario, add proxy scenario dimension (gets dropped again in the end)
  if (length(getItems(data, dim = 3, split = TRUE)) == 1) data <- add_dimension(data, dim = 3.2, add = "scenario")

  # split target variable from drivers
  targetVar   <- data[, , getNames(data, dim = 1)[1]]
  dataDrivers <- data[, , getNames(data, dim = 1)[1], invert = TRUE]

  # set up magpie object for decomposition
  years <- getYears(data, as.integer = TRUE)
  laspeyres <- new.magpie(cells_and_regions = getCells(dataDrivers),
                          years = years[-1],
                          names = getNames(dataDrivers), fill = 0,
                          sets = getSets(dataDrivers))

  # help function to calculate contribution to change of one driver using laspeyres decomposition
  .laspeyresDriver <- function(dataDrivers, targetVar, year, yearBefore, driver) {
    # source: Sun, J. W., Beng Wah Ang. "Some properties of an exact energy decomposition model." Energy 25.12 (2000)
    # formula: Let E be the target variable and X_i the drivers, such that E = \prod_{i=1}^n X_i.
    # Let \Delta X_i := X_i^t - X_i^{t-1}, \Delta E = E^t - E^{t-1}
    # Then \Delta E = \sum_{i=1}^n \Tilde{X_i} with:
    # \Tilde{X_i} := \frac{E^{t-1}}{X_i^{t-1}} \Delta X_i \cdot \left( 1 + \sum_{k = 1}^{n-1}
    #             \left( \frac{1}{k+1} \sum_{i_1 \neq \dots \neq i_k \neq i} \frac{\Delta X_{i_1} \cdots
    #             \Delta X_{i_k}}{X_{i_1}^{t-1} \cdots X_{i_k}^{t-1}} \right) \right)
    # This function calculates \Tilde{X_i} for a given driver i and year t.

    # number of drivers
    n <- length(getNames(dataDrivers))

    # difference from last to this time step in different drivers
    deltas <- dataDrivers[, year, ] - dataDrivers[, yearBefore, ]

    # general factor (combining terms based on current driver)
    generalFactor <- (targetVar[, yearBefore, ] / dataDrivers[, yearBefore, driver]) * deltas[, , driver]

    # remove current driver from vectors of deltas and past values, so we can sample from them
    vectorInnerSumsDeltas <- as.array(deltas[, , driver, invert = TRUE])[1, 1, ]
    vectorInnerSumsPast   <- as.array(dataDrivers[, yearBefore, ][, , driver, invert = TRUE])[1, 1, ]

    # subfunction to calculate inner sum:
    .innerSum <- function(vectorDeltas, vectorPast, k) {
      innerSum <- 0
      for (combination in combn(1:(n - 1), k, simplify = FALSE)) {
        innerSum <- innerSum + prod(vectorDeltas[combination]) / prod(vectorPast[combination])
      }
      return(innerSum)
    }

    # loop adding up outer sum
    outerSum <- 1
    for (k in 1:(n - 1)) {
      outerSum <- outerSum + (1 / (k + 1)) * .innerSum(vectorInnerSumsDeltas, vectorInnerSumsPast, k)
    }

    return(generalFactor * outerSum)
  }

  # loop through drivers and calculate laspeyres decomposition (for all years, regions and scenarios)
  for (reg in getCells(dataDrivers)) {
    for (i in 2:(length(years))) {
      for (scen in getNames(dataDrivers, dim = 2)) {
        for (driver in getNames(dataDrivers, dim = 1)) {
          laspeyres[reg, years[i], list(driver, scen)] <- .laspeyresDriver(dataDrivers[reg, , scen, drop = TRUE],
                                                                           targetVar[reg, , scen],
                                                                           years[i], years[i - 1], driver)
        }
      }
    }
  }


  # add change of target variable back to output object
  targetVarChange <- targetVar[, years[-1], ]
  for (i in 2:(length(years))) targetVarChange[, years[i], ] <- targetVar[, years[i], ] - targetVar[, years[i - 1], ]
  out <- mbind(targetVarChange[, years[-1], ], laspeyres)

  # get relative change
  if (type == "relative") {
    for (i in 2:(length(years))) out[, years[i], ] <- out[, years[i], ] / targetVar[, years[i - 1], ] * 100
  }

  # check decomposition
  diff <- 1 - (out[, , getNames(targetVar, dim = 1)] / dimSums(out[, , getNames(dataDrivers, dim = 1)], dim = 3.1))
  if (any(abs(diff) > 1e-10)) warning(paste0("Decomposition does not add up to target variable. Maximum divergence by ",
                                             round(max(abs(diff)) * 100, 5), "%. Divergence bigger than 1e-10 in ",
                                             sum(abs(diff) > 1e-10), " cases."))

  return(out[, , , drop = TRUE])
}
