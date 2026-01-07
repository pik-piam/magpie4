#' @title reportAgriResearchIntensity
#' @description reports Agricultural Research Intensity as % of total GDP
#'
#' @export
#'
#' @param gdx GDX file
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
reportAgriResearchIntensity <- function(gdx) {
  resInt <- AgriResearchIntensity(gdx, level = "regglo")
  getNames(resInt) <- "Agricultural Research Intensity (% of Total GDP)"
  return(resInt)
}
