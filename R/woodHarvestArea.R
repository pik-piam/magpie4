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
  forestry <- gdx::readGDX(gdx, "ov32_hvarea_forestry", select = list(type = "level"))
  if (all(forestry == 0)) {
    message("Wood harvest area from forestry (ov32_hvarea_forestry) is zero, ",
            "to enable endogenous forestry you can use MAgPIE's scripts/start/forestry.R")
  }
  forestry <- add_dimension(forestry, add = "source", nm = "forestry")

  primforest <- gdx::readGDX(gdx, "ov35_hvarea_primforest", select = list(type = "level"))
  primforest <- add_dimension(primforest, add = "ac", nm = "primary")
  primforest <- add_dimension(primforest, add = "source", nm = "primforest")

  secdforest <- gdx::readGDX(gdx, "ov35_hvarea_secdforest", select = list(type = "level"))
  secdforest <- add_dimension(secdforest, add = "source", nm = "secdforest")

  other <- gdx::readGDX(gdx, "ov35_hvarea_other", select = list(type = "level"))
  other <- add_dimension(other, add = "source", nm = "other")

  x <- mbind(forestry, primforest, secdforest, other)

  # convert from Mha to Mha yr-1
  years <- getYears(x, as.integer = TRUE)
  nYearsPerTimestep <- years[-1] - years[-length(years)]
  stopifnot(nYearsPerTimestep[[1]] == 5)
  nYearsPerTimestep <- c(5, nYearsPerTimestep)
  x <- x / nYearsPerTimestep

  getSets(x) <- c("region", "id", "year", "source", "ageClass")
  getComment(x) <- " unit: Mha yr-1"
  return(x)
}
