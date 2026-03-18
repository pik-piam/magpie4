#' @title landTransitionMatrix
#' @description reads the land transition matrix out of a MAgPIE gdx file.
#' The transition matrix from module 10 (ov_lu_transitions) only captures
#' optimization-driven land conversions. Two processes in module 35 (natveg)
#' shift area across land types in the presolve (before the optimization),
#' bypassing the transition matrix: (1) disturbance losses transferring
#' primary forest to secondary forest (p35_disturbance_loss_primf), and
#' (2) natural regrowth on other land being reclassified as secondary forest
#' once carbon density exceeds 20 tC/ha (p35_maturesecdf). This function adds
#' both flows to the transition matrix
#' to ensure consistency with land stock changes.
#'
#' @importFrom magclass collapseNames dimSums getYears
#' @importFrom memoise memoise
#' @importFrom rlang hash
#' @importFrom R.utils lastModified
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global)
#' or any other aggregation level defined in gdxAggregate
#' @return land transition matrix as MAgPIE object (Mha/yr). The full 7x7 matrix
#' including diagonal (persistence) is returned.
#' @author Florian Humpenoeder
#' @seealso \code{\link{reportLandTransitionMatrix}}
#' @examples
#' \dontrun{
#' x <- landTransitionMatrix(gdx)
#' }
#'
landTransitionMatrix <- memoise(function(gdx, file = NULL, level = "reg") {

  x <- readGDX(gdx, "ov_lu_transitions", select = list(type = "level"))

  if (is.null(x)) {
    warning("Land transition matrix cannot be calculated as ov_lu_transitions could not be found in GDX file! NULL is returned!")
    return(NULL)
  }

  names(dimnames(x))[[3]] <- "landFrom.landTo"

  # Add disturbance losses: primforest -> secdforest (module 35 presolve)
  # These are subtracted from pcm_land("primforest") and added to pc35_secdforest
  # before the optimization, so they are not captured in ov_lu_transitions.
  distPrimf <- readGDX(gdx, "p35_disturbance_loss_primf", react = "silent")
  if (!is.null(distPrimf)) {
    distPrimf <- collapseNames(dimSums(distPrimf, dim = 3))
    # Add to primforest.secdforest and subtract from diagonal primforest.primforest
    x[, , "primforest.secdforest"] <- x[, , "primforest.secdforest"] + distPrimf
    x[, , "primforest.primforest"] <- x[, , "primforest.primforest"] - distPrimf
  }

  # Add natural regrowth: other -> secdforest (module 35 presolve)
  # Young secondary forest on "other" land is reclassified as "secdforest" when
  # carbon density exceeds 20 tC/ha, bypassing the transition matrix.
  matSecdf <- readGDX(gdx, "p35_maturesecdf", react = "silent")
  if (!is.null(matSecdf)) {
    matSecdf <- collapseNames(dimSums(matSecdf, dim = 3))
    # Add to other.secdforest and subtract from diagonal other.other
    x[, , "other.secdforest"] <- x[, , "other.secdforest"] + matSecdf
    x[, , "other.other"] <- x[, , "other.other"] - matSecdf
  }

  x <- gdxAggregate(gdx, x, to = level, absolute = TRUE)

  # Annualize: divide by timestep length to get Mha/yr
  im_years <- collapseNames(m_yeardiff(gdx))
  x <- x / im_years[, getYears(x), ]

  out(x, file)
}, hash = function(x) hash(list(x, getwd(), lastModified(x$gdx))))
