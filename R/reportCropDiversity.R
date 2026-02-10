#' @title reportCropDiversity
#' @description reports crop diversity
#'
#' @export
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Supports "reg", "glo", "regglo", "grid"
#' or a custom region aggregation level.
#' @return Crop diversity as MAgPIE object
#' @author Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- reportCropDiversity(gdx)
#' }
#'
#' @section Crop diversity variables:
#' Name | Unit | Meta
#' ---|---|---
#' Biodiversity\|Shannon crop area diversity index | unitless | Crop type diversity based on area shares (higher = more diverse)
#' Biodiversity\|Inverted Simpson crop area diversity index | unitless | Crop type diversity based on area shares (higher = more diverse)
#' @md

#'
reportCropDiversity <- function(gdx, level = "regglo") {
  if (level %in% c("reg", "glo", "regglo") || isCustomAggregation(level)) {
    a1 <- CropareaDiversityIndex(gdx, index = "shannon", level = level)
    if (!is.null(a1)) getNames(a1) <- "Biodiversity|Shannon crop area diversity index (unitless)" else cat("No crop diversity reporting possible")
    a2 <- CropareaDiversityIndex(gdx, index = "invsimpson", level = level)
    if (!is.null(a2)) getNames(a2) <- "Biodiversity|Inverted Simpson crop area diversity index (unitless)" else cat("No crop diversity reporting possible")
    out <- mbind(a1, a2)
  } else if (level == "grid") {
    a1 <- CropareaDiversityIndex(gdx, index = "shannon", level = "grid")
    if (!is.null(a1)) getNames(a1) <- "Biodiversity|Shannon crop area diversity index (unitless)" else cat("No crop diversity reporting possible")
    a2 <- CropareaDiversityIndex(gdx, index = "invsimpson", level = "grid")
    if (!is.null(a2)) getNames(a2) <- "Biodiversity|Inverted Simpson crop area diversity index (unitless)" else cat("No crop diversity reporting possible")
    out <- mbind(a1, a2)
    out <- metadata_comments(x = out, unit = "unitless", description = "Shannon and Inverted Simpson crop diversity indices", comment = "", note = "")
  } else {
    stop("reportCropDiversity does not support aggregation level: ", level)
  }
  return(out)
}
