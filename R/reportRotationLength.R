#' @title reportRotationLength
#' @description reports Forest rotation length.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Forest rotation length
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportRotationLength(gdx)
#'   }
#' 

reportRotationLength<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    a <- RotationLength(gdx,level = "regglo")
    getNames(a) <- paste0("Rotation lengths|",getNames(a))
  } else {message("NULL returned.")}
  
  return(a)
}