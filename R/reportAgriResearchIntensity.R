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
reportAgriResearchIntensity <- function(gdx) {
  resInt <- AgriResearchIntensity(gdx, level = "regglo")
  getNames(resInt) <- "Agricultural Research Intensity (% of Total GDP)"
  return(resInt)
}
