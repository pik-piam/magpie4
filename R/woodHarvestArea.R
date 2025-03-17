#' woodHarvestArea
#'
#' Reads wood harvest area separated by source (primforest,
#' secdforest, forestry, other) and age classes from a gdx.
#' The data is on cluster level and the unit is Mha per year.
#'
#' @param gdx A fulldata.gdx of a magpie run, usually with endogenous forestry enabled
#' @return A magpie object with the following dimensions: region, id, year, source, ageClass
#' @author Pascal Sauer
#' @export
woodHarvestArea <- function(gdx) {
  x <- harvested_area_timber(gdx, level = "cell", aggregateAgeClasses = FALSE)
  x <- x[, , c("Forestry", "Secondary forest", "Primary forest", "Other land")]
  getSets(x) <- c("region", "id", "year", "source", "ageClass")
  stopifnot(identical(getItems(x, "source"), c("Forestry", "Secondary forest", "Primary forest", "Other land")))
  getItems(x, "source") <- c("forestry", "secdforest", "primforest", "other")
  x <- x[, , c("forestry", "primforest", "secdforest", "other")]
  getComment(x) <- " unit: Mha yr-1"
  return(x)
}
