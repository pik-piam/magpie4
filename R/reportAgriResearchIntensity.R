#' @title reportAgriResearchIntensity
#' @description reports Agricultural Research Intensity as % of total GDP
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @return magpie object
#' @author David Chen
#' @examples
#' \dontrun{
#' x <- reportAgriResearchIntensity(gdx)
#' }
#'
#'
#' @section Agricultural research intensity variables:
#' Name | Unit | Meta
#' ---|---|---
#' Agricultural Research Intensity | % of Total GDP | Share of GDP spent on agricultural research
#' @md

#'
reportAgriResearchIntensity <- function(gdx, level = "regglo") {
  resInt <- AgriResearchIntensity(gdx, level = level)
  getNames(resInt) <- "Agricultural Research Intensity (% of Total GDP)"
  return(resInt)
}
