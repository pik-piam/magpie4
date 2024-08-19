#' woodProduction
#'
#' Reads roundwood and fuelwood production/harvest data separated by source (primforest,
#' secdforest, forestry, other) from a gdx. The data is on cluster level and
#' the unit is Petagram (= mio. t) dry matter per year (Pg DM yr-1).
#'
#' @param gdx A fulldata.gdx of a magpie run, usually with endogenous forestry enabled
#' @return A magpie object with the following dimensions: region, id, year, source, woodType
#' @author Pascal Sauer
#' @export
woodProduction <- function(gdx) {
  timberFromHeaven <- readGDX(gdx, "ov73_prod_heaven_timber", select = list(type = "level"))
  if (any(timberFromHeaven != 0)) {
    warning("Timber production from heaven (ov73_prod_heaven_timber) is not zero.",
            "Please check the MAgPIE run.")
  }
  forestry <- readGDX(gdx, "ov_prod_forestry", select = list(type = "level"))
  if (all(forestry == 0)) {
    message("Timber production from forestry (ov_prod_forestry) is zero, ",
            "to enable endogenous forestry you can use MAgPIE's scripts/start/forestry.R")
  }
  forestry <- add_dimension(forestry, dim = 3.1, add = "land_natveg", "forestry")
  natveg <- readGDX(gdx, "ov_prod_natveg", select = list(type = "level"))
  x <- mbind(forestry, natveg)
  getSets(x) <- c("region", "id", "year", "source", "woodType")
  stopifnot(identical(getItems(x, "woodType"), c("wood", "woodfuel")))
  getItems(x, "woodType") <- c("roundwood", "fuelwood")
  getComment(x) <- " unit: Pg DM yr-1"
  return(x)
}
