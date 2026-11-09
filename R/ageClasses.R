#' @title ageClasses
#' @description Area of secondary forest, forestry (by type of module 32) and other land (othernat and
#' youngsecdf of module 35) by age class.
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level aggregation level, see \code{\link{gdxAggregate}}
#' @return Area in Mha with names "land.type.ac" (type "total" for secondary forest)
#' @author Patrick v. Jeetze
#' @seealso \code{\link{ageClassTransitions}}
#' @examples
#' \dontrun{
#' x <- ageClasses(gdx)
#' }
ageClasses <- function(gdx, file = NULL, level = "cell") {
  # Level of a variable, with the land type added as first item dimension (and the type, if missing).
  .stock <- function(symbol, land, type = NULL) {
    x <- readGDX(gdx, symbol, select = list(type = "level"))
    if (!is.null(type)) {
      x <- add_dimension(x, dim = 3.1, add = "type", nm = type)
    }
    getSets(x, fulldim = FALSE)[3] <- "type.ac"
    return(add_dimension(x, dim = 3.1, add = "land", nm = land))
  }

  x <- mbind(.stock("ov35_secdforest", "secdforest", "total"),
             .stock("ov32_land", "forestry"),
             .stock("ov_land_other", "other"))
  x <- gdxAggregate(gdx, x, to = level, absolute = TRUE)
  out(x, file)
}
