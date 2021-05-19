#' @title reportConsumVal
#' @description reports MAgPIE capital stocks
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#' 
#'   \dontrun{
#'     x <- reportConsumVal(gdx)
#'   }
#' @importFrom magclass getNames

reportConsumVal<-function(gdx){
  
  # Consumption value calculation
  x <- ConsumVal(gdx,level = "regglo")
  
  getNames(x)<-"Value|Consumption Value (million US$05/yr)"

  
  return(x)
  
  
}