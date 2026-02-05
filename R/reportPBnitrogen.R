#' @title reportPBnitrogen
#' @description reports nitrogen planetary boundary
#'
#' @export
#'
#' @param gdx   GDX file
#' @param level level of aggregation (regglo: regions and global)
#'
#' @return MAgPIE object
#' @author Felicitas Beier, Mike Crawford
#' @import magclass
#' @examples
#'
#'   \dontrun{
#'     x <- reportPBnitrogen(gdx)
#'   }
#'
#'
#' @section Nitrogen planetary boundary variables:
#' Name | Unit | Meta
#' ---|---|---
#' Planetary Boundary\|Nitrogen\|Agricultural Nitrogen surplus | Mt N/yr | Total nitrogen surplus from cropland and pasture
#' @md


reportPBnitrogen <- function(gdx, level = "regglo") {

  x <- NULL
  nitrogen <- reportNitrogenPollution(gdx)

  ### Nitrogen Boundary ###
  # (1) Nitrogen Surplus:
  # Critical N surplus: Average value of 25 kg N/ha
  # resolved to the half-degree globally (Schulte-Uebbing et al. 2022)

  indicatorname <- "Planetary Boundary|Nitrogen|Agricultural Nitrogen surplus"
  unit <- "Mt N/yr"
  variable <- paste0(indicatorname, " (", unit, ")")

  # Sum up N surplus
  nitrogen <- dimSums(nitrogen[, , c("Resources|Nitrogen|Pollution|Surplus|+|Cropland (Mt Nr/yr)",
                                     "Resources|Nitrogen|Pollution|Surplus|+|Pasture (Mt Nr/yr)")])

  getItems(nitrogen, dim = 3) <- variable
  x <- mbind(x, nitrogen)

  # Aggregation
  if (!is.null(x)) {
    if (level != "regglo") {
      # Not yet implemented. Requires (dis)aggregation weight.
      # x <- gdxAggregate(gdx, x, to = level, weight = NULL, absolute = TRUE)
    }
    message("Finished calculating Nitrogen PB: Agricultural N surplus")
  }

  return(x)
}
