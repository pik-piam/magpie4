#' @title TimberProductionVolumetric
#' @description reads timber production out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param sumProduct sum over wood and woodfuel (TRUE/FALSE)
#' @param sumSource sum over timber sources: timber plantations, primary forest, secondary forest and non-forest land (woodfuel only) (TRUE/FALSE)
#' @details Annual timber production from timber plantations, primary forest, secondary forest and non-forest land (woodfuel only). Converted from mio. ton DM per year to mio. m3 per year using volumetric conversion factors.
#' @return Timber production in mio. m3 per year
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie setCells add_dimension mbind
#' @examples
#' \dontrun{
#' x <- TimberProductionVolumetric(gdx)
#' }
#'
TimberProductionVolumetric <- function(gdx, file = NULL, level = "regglo", sumProduct = FALSE, sumSource = TRUE) {

  # check timber from heaven
  heaven <- readGDX(gdx, "ov73_prod_heaven_timber", select = list(type = "level"))
  b <- which(heaven > 0, arr.ind = T)
  b <- unique(names(b[, 1]))
  if (!is.null(b)) message(paste0("Missing timber production in ", b, " detected"))

  # combine timber sources
  forestry <- add_dimension(readGDX(gdx, "ov_prod_forestry", select = list(type = "level")), dim = 3.1, add = "source", "forestry")
  natveg <- readGDX(gdx, "ov_prod_natveg", select = list(type = "level"))
  names(dimnames(natveg))[3] <- "source.kforestry"
  a <- mbind(forestry, natveg)

  # convert from DM to m3
  density <- readGDX(gdx, "f73_volumetric_conversion")
  a <- a / density

  # aggregate products
  if (sumProduct) a <- dimSums(a, dim = "kforestry")

  # aggregate sources
  if (sumSource) a <- dimSums(a, dim = "source")

  # aggregate regions
  a <- gdxAggregate(gdx, a, to = level, absolute = TRUE)

  out(a, file)
}
