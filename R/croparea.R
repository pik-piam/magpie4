#' @title croparea
#' @description reads croparea out of a MAgPIE gdx file. Croparea excludes fallow land.
#'
#' @importFrom memoise memoise
#' @importFrom rlang hash
#' @importFrom R.utils lastModified
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#'              "regglo" (regional and global) or
#'              any other aggregation level defined in gdxAggregate
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param water_aggr aggregate irrigated and non-irriagted production or not (boolean).
#' @param physical if TRUE (default) physical (cultivated) area is returned; if FALSE
#'                 harvested area, i.e. physical area scaled by the multicropping ratio.
#' @return production as MAgPIE object (unit depends on attributes)
#' @author Jan Philipp Dietrich, Florian Humpenoeder
#' @seealso \code{\link{reportCroparea}}
#' @examples
#' \dontrun{
#' x <- croparea(gdx)
#' }
#'
#' @importFrom magclass setCells

croparea <- memoise(function(gdx, file = NULL, level = "reg", products = "kcr",
                             product_aggr = TRUE, water_aggr = TRUE, physical = TRUE) {

  if (level %in% c("grid", "iso")) {
    y <- read.magpie(file.path(dirname(normalizePath(gdx)), "cell.land_0.5.mz"))
    x <- read.magpie(file.path(dirname(normalizePath(gdx)), "cell.croparea_0.5_share.mz"))

    if (length(getCells(x)) == "59199") {
      mapfile <- system.file("extdata", "mapping_grid_iso.rds", package = "magpie4")
      mapGridIso <- readRDS(mapfile)
      y <- setCells(y, mapGridIso$grid)
      x <- setCells(x, mapGridIso$grid)
    }
    y <- y[, "y1985", , invert = TRUE] # 1985 is currently the year before simulation start. has to be updated later
    y <- dimSums(y, dim = 3)
    x[is.na(x)] <- 0
    x <- x * y
    fineLevel <- "grid"
  } else {
    x <- readGDX(gdx, "ov_area", format = "first_found",
                 select = list(type = "level"))
    fineLevel <- "cell"
  }

  if (is.null(x)) {
    warning("Crop area cannot be calculated as area data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }

  # applied before any aggregation or subsetting, where the multicropping ratio can be
  # brought to the resolution of the area without needing a weight (relative values are
  # disaggregated evenly)
  if (!physical) {
    multi <- readGDX(gdx, "fm_multicropping", "f18_multicropping", format = "first_found")
    x <- x * gdxAggregate(gdx = gdx, x = multi[, getYears(x), ], weight = NULL,
                          to = fineLevel, absolute = FALSE)
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
  out2 <- gdxAggregate(gdx, x, to = level, weight = "cropland",
                       types = "crop_area", absolute = TRUE)
  out(out2, file)
}
# the following line makes sure that a changing timestamp of the gdx file and
# a working directory change leads to new caching, which is important if the
# function is called with relative path args.
, hash = function(x) hash(list(x, getwd(), lastModified(x$gdx))))
