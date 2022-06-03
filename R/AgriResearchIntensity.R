#' @title AgriResearchIntensity
#' @description calculates Agricultural Research Intensity (Investment in AgR&D/Total GDP)
#' from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, reg, glo or regglo, cell or grid
#' @author David M Chen

#' @examples
#' \dontrun{
#' x <- AgriResearchIntensity(gdx)
#' }
#'
AgriResearchIntensity <- function(gdx, file = NULL, level = "reg") {

  costs <- CostTC(gdx, level = level)

  gdp <- income(gdx, level = level, per_capita = FALSE,
                     type = "mer")

  agInt <- costs / gdp *100

  out(agInt, file)
}
