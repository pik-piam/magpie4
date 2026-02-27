#' @title readGDXBilateral
#' @description Internal support function to read bilateral spatial data from GDX files.
#' @param gdx gdx file to report from
#' @param symbol name of the symbol to read from gdx
#' @author Pascal Sauer, David M Chen
#' 
readGDXBilateral <- function(gdx, symbol) {
  x <- gamstransfer::readGDX(gdx, symbol)
  x <- magclass::as.magpie(x[[symbol]]$records)
  x[is.na(x)] <- 0
  return(x)
}
