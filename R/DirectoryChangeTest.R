#' @title DirectoryChangeTest
#' @description Tests whether the working directory has been changed during the R session
#'
#' @importFrom memoise memoise
#'
#' @return stops script on case of working directory changed
#' @author Benjamin Leon Bodirsky
#'
directoryChangeTest <- function() {
  getwdx <- memoise(function() {
    getwd()
  })
  if (getwdx() != getwd()) {
    stop("Working directory has been changed during R session,
         conflicts with caching via memoise in magpie4")
  }
}
