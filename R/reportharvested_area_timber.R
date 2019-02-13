#' @title reportharvested_area_timber
#' @description reports MAgPIE harvested area for timber.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Area harvested for timber production
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportharvested_area_timber(gdx)
#'   }
#' 

reportharvested_area_timber<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    a <- harvested_area_timber(gdx,level = "regglo")
    a <- mbind(a,setNames(dimSums(a,dim=3),"Total"))
    getNames(a) <- paste0("harvested area for timber|",getNames(a))
    getNames(a) <- paste0(getNames(a)," (million ha)")
  } else {cat("NULL returned for magpie run without dynamic forestry.")}
  
  return(a)
}