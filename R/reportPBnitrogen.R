#' @title reportPBnitrogen
#' @description reports nitrogen planetary boundary
#'
#' @export
#'
#' @param gdx GDX file
#' @param level level of aggregation (regglo: regions and global)
#' @param dir directory with required spatial data
#'
#' @return MAgPIE object
#' @author Mike Crawford, Felicitas Beier
#' @import magclass
#' @examples
#'
#'   \dontrun{
#'     x <- reportPBnitrogen(gdx)
#'   }
#'

reportPBnitrogen <- function(gdx, level = "regglo", dir = ".") {

  x <- NULL
  nitrogen <- reportNitrogenPollution(gdx, dir = dir)

  ### Nitrogen Boundary ###
  # (1) Nitrogen Surplus:
  # Critical N surplus: Average value of 25 kg N/ha
  # resolved to the half-degree globally (Schulte-Uebbing et al. 2022)
  # MIKE: Nitrogen surplus or exceedance? Calculate from spatial level and aggregate to global?
  # (that's how we plan to do it e.g. for Biosphere and Land boundaries)

  indicatorname <- "Planetary Boundary|Nitrogen|Nitrogen surplus"
  unit <- "Mt N/yr"
  variable <- paste0(indicatorname, " (",unit,")")

  # Sum up N surplus
  nitrogen[, , variable] <- dimSums(nitrogen[, , c("Resources|Nitrogen|Pollution|Surplus|+|Cropland (Mt Nr/yr)",
                                                   "Resources|Nitrogen|Pollution|Surplus|+|Pasture (Mt Nr/yr)",
                                                   "Resources|Nitrogen|Pollution|Surplus|+|Animal Waste Management (Mt Nr/yr)")])
  getItems(nitrogen, dim = 3) <- variable
  x <- mbind(x, nitrogen)

  return(x)
}
