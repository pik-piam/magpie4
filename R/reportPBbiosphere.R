#' @title reportPBbiosphere
#' @description reports biosphere planetary boundary:
#'              global land surface covered by largely intact natural areas
#'
#' @export
#'
#' @param gdx GDX file
#' @param level level of aggregation (regglo: regions and global)
#' @param dir directory with required spatial data
#'
#' @return MAgPIE object
#' @author Patrick von Jeetze, Felicitas Beier
#' @import magclass
#' @examples
#'
#'   \dontrun{
#'     x <- reportPBbiosphere(gdx)
#'   }
#'

reportPBbiosphere <- function(gdx, level = "regglo", dir = ".") {

  land <- reportLandUse(gdx)

  ### Biosphere Boundary ###
  # (1) Area of largely intact natural area:
  # Rockstroem et al. (2023): 50-60% of global land surface area
  indicatorname <- "Planetary Boundary|Biosphere|Largely intact natural area"
  unit <- "Mha"
  variable <- paste0(indicatorname, " (", unit, ")")

  # Sum up land classes
  x <- dimSums(land[, , c("Resources|Land Cover|+|Other Land (million ha)",
                          "Resources|Land Cover|+|Forest (million ha)")], # Patrick: all forest or only natural (because "largely intact")?
                                                                          # or same as in land boundary? to be consistent?
               dim = 3)
  getItems(x, dim = 3) <- variable

  return(x)
}
