#' @title cropland
#' @description reads cropland out of a MAgPIE gdx file. Cropland includes croparea plus fallow plus cropland with tree cover
#'
#' @importFrom magclass mbind read.magpie
#' @importFrom memoise memoise
#' @importFrom rlang hash
#' @importFrom R.utils lastModified
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global)
#' or any other aggregation level defined in gdxAggregate
#' @param types NULL or a vector of strings. If NULL, all land types are used. Options are
#' "crop_area", "crop_fallow", "crop_treecover"
#' @param sum determines whether output should be land-type-specific (FALSE) or aggregated over all types (TRUE).
#' @return land as MAgPIE object (Mha)
#' @author Benjamin Leon Bodirsky
#' @seealso \code{\link{reportLandUse}}
#' @examples
#' \dontrun{
#' x <- cropland(gdx)
#' }
#'
#' @importFrom magclass setCells


cropland <- memoise(function(gdx, level = "reg", types = NULL, sum = FALSE) {
  x <- land(gdx = gdx, level = level, types = NULL, subcategories = NULL, sum = FALSE)

  cropland <- x[,,"crop"]
  fallow_land <- fallow(gdx = gdx, level = level)
  croptree_land <- croplandTreeCover(gdx, level = level)

  croparea_land <- setNames(cropland - setNames(fallow_land, NULL) - setNames(croptree_land, NULL), "crop_area")

  crop <- mbind(croparea_land,
                fallow_land,
                croptree_land)

  if (abs(sum(x[, , "crop"] - dimSums(crop, dim = 3))) > 2e-05) {
    warning("Cropland: Total and sum of subcategory land types diverge!")
  }
  if (any(crop < -10^-9)) {
    if (level == "grid") {
      warning("Negative areas. Fallow and Cropland Tree Cover exceed cropland. This can happen due to grid disaggregation.")
    } else {
      stop("Negative areas. Fallow and Cropland Tree Cover exceed cropland.")
    }
  } else {
    # small negative areas can occur due to rounding
    crop[crop < 0] <- 0
  }

  if (!is.null(types)) {
    x <- x[, , types]
  }

  if (sum) {
    crop <- dimSums(crop, dim = 3)
    getNames(crop) <- "sum"
  }

  return(crop)
}
# the following line makes sure that a changing timestamp of the gdx file and
# a working directory change leads to new caching, which is important if the
# function is called with relative path args.
,hash = function(x) hash(list(x, getwd(), lastModified(x$gdx))))


