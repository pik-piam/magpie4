#' @title landTransitionMatrix
#' @description reads the land transition matrix out of a MAgPIE gdx file
#'
#' @importFrom magclass collapseNames
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

  x <- gdxAggregate(gdx, x, to = level, absolute = TRUE)

  # Annualize: divide by timestep length to get Mha/yr
  im_years <- collapseNames(m_yeardiff(gdx))
  x <- x / im_years[, getYears(x), ]

  out(x, file)
}, hash = function(x) hash(list(x, getwd(), lastModified(x$gdx))))
