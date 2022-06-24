#' @title reportBII
#' @description reports biodiversity intactness index
#'
#' @export
#'
#' @param gdx GDX file
#' @return Biodiversity intactness index as MAgPIE object
#' @author Patrick v. Jeetze, Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportBII(gdx)
#'   }
#'

reportBII <- function(gdx) {
  a <- BII(gdx, level = "regglo")
  if(!is.null(a)) getNames(a) <- "Biodiversity|BII (unitless)" else cat("No biodiversity reporting possible")
  b1 <- CropareaDiversityIndex(gdx, index="shannon", level = "regglo")
  if(!is.null(b1)) getNames(b1) <- "Biodiversity|Shannon croparea diversity index (unitless)" else cat("No biodiversity reporting possible")
  b2 <- CropareaDiversityIndex(gdx, index="invsimpson", level = "regglo")
  if(!is.null(b2)) getNames(b2) <- "Biodiversity|Inverted Simpson croparea diversity index (unitless)" else cat("No biodiversity reporting possible")

  out=mbind(a,b1,b2)
  return(out)
}

