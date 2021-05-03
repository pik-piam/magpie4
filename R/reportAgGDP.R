#' @title reportAgGDP
#' @description reports MAgPIE Agricultural GDP Mio. USD05 MER
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Magpie object 
#' @author Edna J. Molina Bacca
#' @examples
#' 
#'   \dontrun{
#'     x <- reportAgGDP(gdx)
#'   }
#' @importFrom magclass getNames

reportAgGDP<-function(gdx){
  
  #Capital stocks used in croland per region 
  x <- AgGDP(gdx,level = "regglo")
  getNames(x)<-"Value|Agriculture GDP" 

  
  return(x)
  
  
}