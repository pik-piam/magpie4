#' @title reportPBwater
#' @description reports water planetary boundaries
#'
#' @export
#'
#' @param gdx GDX file
#' @param level level of aggregation (regglo: regions and global)
#'
#' @return MAgPIE object
#' @author Felicitas Beier, Jens Heinke
#' @import magclass
#' @examples
#'
#'   \dontrun{
#'     x <- reportPBwater(gdx)
#'   }
#'

reportPBwater <- function(gdx, level = "regglo") {

  x <- NULL
  waterUse <- reportWaterUsage(gdx, detail = FALSE)

  ### Blue Water Boundary ###
  # (1) Total water consumption (Rockstroem et al. 2009: 4000 km3; Gerten et al. 2013: 2800 km3)
  indicatorname <- "Planetary Boundary|Freshwater|Water consumption"
  unit <- "km3/yr"

  # MAgPIE variable: agricultural water withdrawal transformed to water consumption using
  # assumption from Jaegermeyr et al. (50% of withdrawal is consumptive)
  # and exogenous non-agricultural water consumption scenario from WATERGAP
  waterWC <- collapseNames(waterUse[, , "Resources|Water|Consumption|Agriculture (km3/yr)"] +
                            waterUse[, , "Resources|Water|Consumption|Non-agriculture (km3/yr)"])
  getItems(waterWC, dim = 3) <- paste0(indicatorname, " (", unit, ")")
  x <- mbind(x, waterWC)

  # # (2) Environmental flow violation volume
  # # This indicator is motivated by the Rockstroem et al. (2023) indicator
  # # (<20% magnitude monthly surface flow alteration in all grid cells)
  # efvVolume <- waterEFViolation(gdx, level = "regglo", digits = 4)
  # indicatorname <- "Planetary Boundary|Freshwater|Environmental flow violation volume"
  # unit          <- "km3/yr"
  #
  # ### Build in check whether correct EFR scenario is chosen. Otherwise: put NA in variable
  # getItems(efvVolume, dim = 3) <- paste0(indicatorname, " (", unit, ")")
  # x <- mbind(x, efvVolume)

  ### JENS: Should we report the EFV volume or the area that experiences EFV?
  # Or better none because none of them is really what they expect to be reported?

  # (3) Richardson et al. (2023):
  # Upper limit (95th percentile) of global land area with deviations greater than during preindustrial
  # This is not yet implemented in MAgPIE. It requires:
  # (a) LPJmL runs including the preindustrial period
  # (b) downscaling of water withdrawals from MAgPIE using mrwater

  ### Green Water Boundary ###
  # The green water boundary cannot be reported with MAgPIE alone.
  # This would require coupling to LPJmL/a global hydrological (vegetation) model.

  # Aggregation
  if (!is.null(x)) {
    if (level != "regglo") {
      # Not yet implemented. Requires (dis)aggregation weight.
      # x <- gdxAggregate(gdx, x, to = level, weight = NULL, absolute = TRUE)
    }
    message("Finished calculating Water PB: Total water consumption")
  }

  return(x)
}
