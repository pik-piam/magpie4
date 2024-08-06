#' @title tryList
#' @description Internal support function to run a list of reportings in
#' a \code{\link{tryReport}} environment.
#'
#' @param ... report function to be run
#' @param gdx gdx file to report from
#' @param level spatial level (either "regglo" for region+global or "iso" for ISO countries)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{tryReport}}

tryList <- function(..., gdx, level = "regglo") {
  width <- max(nchar(c(...))) + 1
  return(lapply(unique(list(...)), tryReport, width, gdx, level = level, n = 2))
}
