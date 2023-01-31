#' @title reportKBAIntactness
#' @description reports key biodiversity area intactness index
#'
#' @export
#'
#' @param gdx GDX file
#' @param dir magpie output directory that contains gridded BII data
#' @return Key biodiversity area intactness index as MAgPIE object
#' @author Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- reportKBAIntactness(gdx)
#' }
#'
reportKBAIntactness <- function(gdx, dir = ".") {
  KBAarea <- c("input/kba_land_0.5.mz",
                 "../input/kba_land_0.5.mz",
                 "../../input/kba_land_0.5.mz")
  KBAarea <- suppressWarnings(KBAarea[min(which(file.exists(KBAarea)))])
  KBAarea <- read.magpie(KBAarea)

  a <- BII(gdx, level = "regglo", mode = "from_grid",
           adjusted = TRUE, spatialWeight = KBAarea, dir = dir)
  if (!is.null(a)) getNames(a) <- "Biodiversity|Key biodiversity area intactness (unitless)" else cat("Key biodiversity area reporting possible")
  out <- a
  return(out)
}

