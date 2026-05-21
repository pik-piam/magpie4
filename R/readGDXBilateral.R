#' @title readGDXBilateral
#' @description Internal support function to read bilateral spatial data from GDX files.
#' @param gdx gdx file to report from
#' @param symbol name of the symbol to read from gdx
#' @importFrom dplyr relocate
#' @author David M Chen
#' 
readGDXBilateral <- function(gdx, symbol) {
  x <- suppressWarnings(readGDX(gdx, symbol))
  if(is.null(x)) {
  return(NULL)
  }
  x <- as.data.frame(x, rev = 2)
  x <- dplyr::relocate(x, "i_im", .before = 2)
  x <- as.magpie(x, spatial = c(1,2), temporal = 3, tidy = TRUE)
  return(x)
}
