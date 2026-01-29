#' sdgIndicator
#'
#' Helper function that renames SDG data according to a fixed naming scheme.
#'
#' @param indicatorName The name of the SDG indicator variable (e.g., "SDG|SDG02|Food availability")
#' @param unit The name of the unit to be used (e.g., "kcal/cap/day")
#' @param data A magpie object containing the actual indicator data
#' @return Returns a magpie object with the correct name
#' @keywods internal
sdgIndicator <- function(indicatorName, unit, data) {
  getNames(data) <- paste0(indicatorName, " (", unit, ")")
  return(data)
}