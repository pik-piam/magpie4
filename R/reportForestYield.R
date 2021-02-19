#' @title reportForestYield
#' @description reports MAgPIE harvested area for timber.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Yield from Forests for timber production
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportForestYield(gdx)
#'   }
#' 

reportForestYield<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    a_harvest <- ForestYield(gdx,level = "regglo")
    if(is.null(a_harvest)){
      cat("Forest yields not reported for runs without timber production. ")
    } else {
      getNames(a_harvest) <- paste0("Timber Yields|Harvest|",getNames(a_harvest))
      getNames(a_harvest) <- paste0(getNames(a_harvest)," (m3 per ha)")
      a <- a_harvest
    }
  } else {cat("Disabled for magpie run without timber production.")}
  
  return(a)
}