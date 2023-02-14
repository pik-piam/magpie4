#' @title reportAgLandscapeIntactness
#' @description reports agricultural landscape intactness index
#'
#' @export
#'
#' @param gdx GDX file
#' @param dir magpie output directory that contains gridded BII data
#' @return Agricultural landscape intactness index as MAgPIE object
#' @author Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- reportAgLandscapeIntactness(gdx)
#' }
#'
reportAgLandscapeIntactness <- function(gdx, dir = ".") {
  cropland <- land(gdx = gdx, level = "grid", types = "crop", dir = dir)
  # Set minuscule values of cropland (< 10 ha per grid cell) to zero
  cropland[cropland < 0.0001] <- 0
  a <- BII(gdx, level = "regglo", mode = "from_grid",
           adjusted = TRUE, spatialWeight = cropland, dir = dir)
  if (!is.null(a)) getNames(a) <- "Biodiversity|Agricultural landscape intactness (unitless)" else cat("No Ag. landscape intactness reporting possible")
  out <- a
  return(out)
}

