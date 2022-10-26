#' @title reportCropDiversity
#' @description reports crop diversity
#'
#' @export
#'
#' @param gdx GDX file
#' @param grid Set to TRUE, if outputs should be reported on 0.5 degree grid level
#' @return Crop diversity as MAgPIE object
#' @author Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- reportCropDiversity(gdx)
#' }
#'
reportCropDiversity <- function(gdx, grid = FALSE) {
  if (grid == FALSE) {
    a1 <- CropareaDiversityIndex(gdx, index = "shannon", level = "regglo")
    if (!is.null(a1)) getNames(a1) <- "Biodiversity|Shannon crop area diversity index (unitless)" else cat("No crop diversity reporting possible")
    a2 <- CropareaDiversityIndex(gdx, index = "invsimpson", level = "regglo")
    if (!is.null(a2)) getNames(a2) <- "Biodiversity|Inverted Simpson crop area diversity index (unitless)" else cat("No crop diversity reporting possible")
    out <- mbind(a1, a2)
  } else {
    a1 <- CropareaDiversityIndex(gdx, index = "shannon", level = "cell")
    a1 <- gdxAggregate(gdx, a1, to = "grid", absolute = FALSE)
    if (!is.null(a1)) getNames(a1) <- "Biodiversity|Shannon crop area diversity index (unitless)" else cat("No crop diversity reporting possible")
    a2 <- CropareaDiversityIndex(gdx, index = "invsimpson", level = "cell")
    a2 <- gdxAggregate(gdx, a2, to = "grid", absolute = FALSE)
    if (!is.null(a2)) getNames(a2) <- "Biodiversity|Inverted Simpson crop area diversity index (unitless)" else cat("No crop diversity reporting possible")
    out <- mbind(a1, a2)
    out <- metadata_comments(x = out, unit = "unitless", description = "Shannon and Inverted Simpson crop diversity indices", comment = "", note = "")
  }
  return(out)
}
