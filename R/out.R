#' out
#'
#' Function to safely returns parameters. Function returns either the output or
#' writes it to a file. Please use this function when you write own GDX output
#' functions.
#'
#'
#' @usage out(x,file)
#' @param x an object that can be converted to a MAgPIE object
#' @param file file name of a file it should be written to. NULL, if x should
#' be returned instead to be written to a file.
#' @return NULL or x as MAgPIE object
#' @author Jan Philipp Dietrich
#' @importFrom magclass as.magpie write.magpie

out <- function(x, file) {
  if (is.null(file)) {
    return(as.magpie(x))
  } else {
    if (is.null(x)) {
      write.magpie(as.magpie(numeric(0)), file)
    } else {
      write.magpie(as.magpie(x), file)
    }
  }
}
