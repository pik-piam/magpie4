#' @title reportPBland
#' @description reports land planetary boundary: forest area as percentage of original
#'              forest area
#'
#' @export
#'
#' @param gdx GDX file
#' @param level level of aggregation (regglo: regions and global)
#' @param dir directory with required spatial data
#'
#' @return MAgPIE object
#' @author Felicitas Beier
#' @import magclass
#' @examples
#'
#'   \dontrun{
#'     x <- reportPBland(gdx)
#'   }
#'

reportPBland <- function(gdx, level = "regglo", dir = ".") {

  land <- reportLandUse(gdx)

  ### Land Boundary ###
  # (1) Area of forested land (compared to original forest cover):
  # Richardson et al. (2023): 75% of original forest cover
  indicatorname <- "Planetary Boundary|Land|Forest cover"
  unit <- "Mha"
  variable <- paste0(indicatorname, " (", unit, ")")

  # Select forest categories that count towards forested land
  x <- collapseNames(land[, , "Resources|Land Cover|+|Forest (million ha)"])
  getItems(x, dim = 3) <- variable

  # Patrick/Florian: Which ones to include?
  # What does "Resources|Land Cover|+|Forest (million ha)" contain?
  # "Resources|Land Cover|Forest|+|Natural Forest (million ha)"
  # "Resources|Land Cover|Forest|Natural Forest|+|Primary Forest (million ha)"
  # "Resources|Land Cover|Forest|Natural Forest|+|Secondary Forest (million ha)"
  # "Resources|Land Cover|Forest|+|Managed Forest (million ha)"
  # "Resources|Land Cover|Agricultural land (million ha)"
  # "Resources|Land Cover|Forest|Managed Forest|+|Plantations (million ha)"
  # "Resources|Land Cover|Forest|Managed Forest|+|NPI/NDC (million ha)"
  # "Resources|Land Cover|Forest|Managed Forest|+|Afforestation (million ha)"

  return(x)
}
