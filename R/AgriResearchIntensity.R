#' @title AgriResearchIntensity
#' @description calculates Agricultural Research Intensity (Investment in AgR&D/Total GDP)
#' from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, reg, glo or regglo, cell or grid
#' @param debug debug mode TRUE makes some consistency checks between estimates for different resolutions.
#' @author David M Chen

#' @examples
#' \dontrun{
#' x <- AgriResearchIntensity(gdx)
#' }
#'
AgriResearchIntensity <- function(gdx, file = NULL, level = "reg",  debug = FALSE) {

  costs <- CostTC(gdx, level = level)

  gdp <- income(gdx, level = level, per_capita = FALSE,
                     type = "mer")

  out <- costs/gdp *100

  return(out, file)
}
