#' @title reportFaustmannRotationLength
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
#'     x <- reportFaustmannRotationLength(gdx)
#'   }
#' 

reportFaustmannRotationLength<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"p32_max_npv_rotation")))){
    a <- FaustmannRotationLength(gdx,level = "regglo")
    getNames(a) <- paste0("Rotation lengths|",getNames(a))
  } else {message("NULL returned.")}
  
  return(a)
}