#' @title croparea
#' @description reads croparea out of a MAgPIE gdx file. Croparea excludes fallow land.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#'              "regglo" (regional and global) or
#'              any other aggregation level defined in superAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param water_aggr aggregate irrigated and non-irriagted production or not (boolean).
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @return production as MAgPIE object (unit depends on attributes)
#' @author Jan Philipp Dietrich, Florian Humpenoeder
#' @seealso \code{\link{reportCroparea}}
#' @examples
#' \dontrun{
#' x <- croparea(gdx)
#' }
#'
#' @importFrom magclass setCells

croparea <- function(gdx, file = NULL, level = "reg", products = "kcr",
                     product_aggr = TRUE, water_aggr = TRUE, dir = ".", spamfiledirectory = "") {

  dir <- getDirectory(dir, spamfiledirectory)

  if (level == "grid") {
    mapfile <- system.file("extdata", "mapping_grid_iso.rds", package="magpie4")
    map_grid_iso <- readRDS(mapfile)
    y <- setCells(read.magpie(file.path(dir, "cell.land_0.5.mz")), map_grid_iso$grid)
    y <- y[, "y1985", , invert = TRUE] # 1985 is currently the year before simulation start. has to be updated later
    y <- dimSums(y, dim = 3)
    x <- setCells(read.magpie(file.path(dir, "cell.croparea_0.5_share.mz")), map_grid_iso$grid)
    x[is.na(x)] <- 0
    x <- x * y
  } else {
    x <- readGDX(gdx, "ov_area", format = "first_found",
                 select = list(type = "level"))
  }

  if (is.null(x)) {
    warning("Crop area cannot be calculated as area data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }
  if (water_aggr) {
    x <- dimSums(x, dim = 3.2)
  }
  if (!all(products %in% findset("kcr"))) {
    products <- readGDX(gdx, products)
  }
  x <- x[, , products]
  if (product_aggr) {
    x <- dimSums(x, dim = 3.1)
  }
  out <- gdxAggregate(gdx, x, to = level,
                      weight = "land", type = "crop", absolute = TRUE,
                      dir = dir)
  out(out, file)
}
