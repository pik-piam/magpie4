#' @title reportWorkingAgePopulation
#' @description reports working age population
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return working age population as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportWorkingAgePopulation(gdx)
#'   }
#'

reportWorkingAgePopulation <- function(gdx, level = "regglo") {

  # read in data
  workingAge <- c("15--19", "20--24", "25--29", "30--34", "35--39", "40--44",
                        "45--49", "50--54", "55--59", "60--64")
  population <- dimSums(population(gdx, level = level, age = TRUE)[, , workingAge], dim = 3)

  # rename
  getNames(population) <- "Working age population (million people)"

  return(population)
}

