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
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"ov44_biodiv")))) {
    a <- BII(gdx, level = "regglo")
    getNames(a) <- "Biodiversity|BII (unitless)"
  } 
  else {
    cat("No BII reported for runs without biodiversity (default). ") }
  
  return(a)
}

