#' @title waterStressedPopulation
#'
#' @description People living in water stressed region
#'
#' @param gdx         GDX file
#' @param file        a file name the output should be written to using write.magpie
#' @param level       spatial level of aggregation: "cell" (cellular), "reg" (regional),
#'                    "glo" (global), "regglo" (regional and global) or
#'                    "grid" (grid cell)
#' @param absolute    TRUE: reports people living in water stressed region in million,
#'                    FALSE: returns share of population
#'
#' @return MAgPIE object
#'
#' @export
#'
#' @author Felicitas Beier
#' @examples
#'
#'   \dontrun{
#'     x <- waterStressRatio(gdx)
#'   }
#'

waterStressedPopulation <- function(gdx, file = NULL, level = "cell",
                                    absolute = TRUE) {

  # Def.: number of people living in water stressed region
  watStress <- waterStress(gdx, stressRatio = 0.4, level = "cell")
  pop       <- suppressWarnings(population(gdx, level = "cell"))

  if (absolute) {

    out <- pop * watStress
    out <- gdxAggregate(gdx, x = out, to = level, absolute = TRUE,
                        weight = "population")
  } else {

    out <- pop * watStress / pop
    out <- gdxAggregate(gdx, x = out, to = level, absolute = FALSE,
                        weight = "population")
  }

  ### Limitations:
  # highly dependent on chosen threshold for "water stress"
  # disaggregation of population introduces bias
  # not everyone who lives in water stressed region is water stressed

  return(out)
}
