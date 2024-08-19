#' @title reportCostsPresolve
#' @description reports MAgPIE costs
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return consumption value as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCostsPresolve(gdx)
#'   }
#' 

reportCostsPresolve<-function(gdx){
  
  a <- costsPresolve(gdx,level = "regglo")
  if(!is.null(a)) getNames(a) <- "Costs|PreSolve|Total (million US$2017)"
  
  return(a)
}