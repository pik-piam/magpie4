#' @title tryList
#' @description Internal support function to run a list of reportings in
#' a \code{\link{tryReport}} environment.
#'
#' @param ... report function to be run
#' @param gdx gdx file to report from
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{tryReport}}

tryList <- function(..., gdx) {
  width <- max(nchar(c(...))) + 1
  return(lapply(unique(list(...)), tryReport, width, gdx, n = 2))
}
