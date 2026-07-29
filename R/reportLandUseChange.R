#' @title reportLandUseChange
#' @description reports land-use change
#'
#' @export
#'
#' @param gdx GDX file
#' @param baseyear baseyear for calculating land-use change. Ignored if annual is TRUE.
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @param annual If FALSE (default), report cumulative land-use change relative to baseyear
#' (million ha wrt baseyear) for all land types. If TRUE, report the average annual net change
#' rate per time step (million ha/yr) for primary forest and natural planted forest. The first
#' time step has no preceding time step and is set to 0.
#' @return land-use change as MAgPIE object (million ha wrt to baseyear, or million ha/yr if
#' annual is TRUE)
#' @author Florian Humpenoeder, Miodrag Stevanović
#' @examples
#'
#'   \dontrun{
#'     x <- reportLandUseChange(gdx)
#'   }
#'
#' @section Land-use change variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover Change\|Cropland | million ha wrt baseyear | Change in cropland area relative to baseyear
#' Resources\|Land Cover Change\|Pastures and Rangelands | million ha wrt baseyear | Change in pasture area relative to baseyear
#' Resources\|Land Cover Change\|Forest | million ha wrt baseyear | Change in forest area relative to baseyear
#' Resources\|Land Cover Change\|Other Land | million ha wrt baseyear | Change in other land area relative to baseyear
#'
#' @section Annual land-use change variables (annual = TRUE):
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Land Cover Change\|Forest\|Natural Forest\|Primary Forest\|Annual | million ha/yr | Average annual net change in primary forest area over the time step
#' Resources\|Land Cover Change\|Forest\|Planted Forest\|Natural\|Annual | million ha/yr | Average annual net change in natural planted forest area over the time step
#' @md
reportLandUseChange <- function(gdx, baseyear = 1995, level = "regglo", annual = FALSE) {

  #get LandUse
  x <- reportLandUse(gdx, level = level)

  #drop variables
  x <- x[, , "Resources|Land Cover (million ha)", invert = TRUE]

  if (annual) {

    # Land types reported as annual net-change rates: primary forest and "natural" (non-plantation)
    # planted forest. The planted-forest split replaced the old "Planted Forest|+|Natural" line with
    # its components (CO2-price AR + NPI_NDC AR + Other Planted); reconstruct that aggregate here so
    # the annual-change series is preserved and stays comparable to develop (where these pools were
    # lumped together). intersect() keeps it robust to gdx that lack individual components.
    primary <- "Resources|Land Cover|Forest|Natural Forest|+|Primary Forest (million ha)"
    naturalPlantedCandidates <- c(
      "Resources|Land Cover|Forest|Planted Forest|+|CO2-price AR (million ha)",
      "Resources|Land Cover|Forest|Planted Forest|+|NPI_NDC AR (million ha)",
      "Resources|Land Cover|Forest|Planted Forest|+|Other Planted (million ha)"
    )
    naturalPlantedParts <- intersect(naturalPlantedCandidates, getNames(x))
    xPrimary <- x[, , primary]
    if (length(naturalPlantedParts)) {
      naturalPlanted <- setNames(dimSums(x[, , naturalPlantedParts], dim = 3),
                                 "Resources|Land Cover|Forest|Planted Forest|+|Natural (million ha)")
      x <- mbind(xPrimary, naturalPlanted)
    } else {
      x <- xPrimary
    }

    #calc change between consecutive time steps, attributed to the later year
    y <- getYears(x)
    firstYear <- x[, 1, ]
    firstYear[, , ] <- 0
    x <- x[, y[-1], ] - setYears(x[, y[-length(y)], ], y[-1])

    #divide by time step length to get annual rates and add back the first time step
    x <- x / collapseNames(m_yeardiff(gdx))[, getYears(x), ]
    x <- mbind(firstYear, x)

    #rename variable and unit. The summation symbols are dropped because the annual rates do not
    #belong to the summation groups of the corresponding cumulative variables.
    getNames(x) <- gsub("\\|Land Cover\\|", "\\|Land Cover Change\\|", getNames(x))
    getNames(x) <- gsub("\\|\\+\\|", "\\|", getNames(x))
    getNames(x) <- gsub(" \\(million ha\\)", "\\|Annual \\(million ha/yr\\)", getNames(x))

  } else {

    #calc land-use change wrt to baseyear
    x <- x - setYears(x[, baseyear, ], NULL)

    #rename variable and unit
    getNames(x) <- gsub("\\|Land Cover\\|", "\\|Land Cover Change\\|", getNames(x))
    getNames(x) <- gsub("\\(million ha\\)", paste0("\\(million ha wrt ", baseyear, "\\)"), getNames(x))
  }

  return(x)
}
