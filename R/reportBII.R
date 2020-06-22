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
  return(a)
}

