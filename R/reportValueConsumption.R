#' @title reportValueConsumption
#' @description reports consumption value
#' 
#' @importFrom magpiesets reporthelper 
#' @export
#' 
#' @param gdx GDX file
#' @param detail if true, provides estimates for all commodities, otherwhise aggregates some groups
#' @return consumption value as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportValueConsumption(gdx)
#'   }
#' 

reportValueConsumption<-function(gdx,detail=FALSE){
  
  x <- NULL
  
  # consumption Value
  out<-collapseNames(consumValue(gdx, level="regglo",products="kfo"))
  out<-reporthelper(x=out,dim = 3.1,level_zero_name = "Food Consumption Value",detail = FALSE)
  getNames(out) <- paste(getNames(out),"(million US$05/yr)",sep=" ")
  x <- mbind(x,out)
  
  # food expenditure share
  out<-collapseNames(consumValue(gdx, level="regglo",expenditure_shr = TRUE))
  out<-reporthelper(x=out,dim = 3.1,level_zero_name = "Food Expenditure Share",detail = FALSE)
  getNames(out) <- paste(getNames(out),"(% of GDP)",sep=" ")
  x <- mbind(x,out)

  return(x)
}