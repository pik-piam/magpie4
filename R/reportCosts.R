#' @title reportCosts
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
#'     x <- reportCosts(gdx)
#'   }
#' 

reportCosts<-function(gdx){

  a <- costs(gdx,level = "regglo",sum = FALSE)
  getNames(a) <- paste0("Costs|MainSolve|",getNames(a))
  
  x <- NULL
  x <- mbind(x,setNames(dimSums(a,dim=3),"Costs|MainSolve"))
  x <- mbind(x,a)
  x <- mbind(x,setNames(dimSums(a[,,"Costs|MainSolve|GHG Emissions",invert=TRUE],dim=3),"Costs|MainSolve w/o GHG Emissions"))
  
  getNames(x) <- paste0(getNames(x)," (million US$05/yr)")
  
  return(x)
}