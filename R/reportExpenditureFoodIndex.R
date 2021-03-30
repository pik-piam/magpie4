#' @title reportExpenditureFoodIndex
#' @description reports food expenditure index and food expenditure index corrected for emission costs
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param baseyear   Baseyear of the price index
#' @param basketyear Year of reference food basket (should be in the past for comparison of different runs to have identical and comparable food basket)
#' 
#' @return Food expenditure index as MAgPIE object 
#' @author Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceFoodIndex(gdx)
#'   }
#' 

reportExpenditureFoodIndex <- function(gdx, baseyear="y2010", basketyear="y2010") {

  # corrected food expenditure index all food products
  x1 <- expenditureIndexFood(gdx, level="regglo", basketyear=basketyear, baseyear=baseyear, ghgtax=TRUE, round=TRUE)
  getNames(x1) <- paste0("Prices|Food Expenditure Index corrected for ghg costs (Index ",gsub("\\y","",baseyear),"=100)")
  
  # food expenditure index all food products
  x2 <- expenditureIndexFood(gdx, level="regglo", basketyear=basketyear, baseyear=baseyear, ghgtax=FALSE, round=TRUE)
  getNames(x2) <- paste0("Prices|Food Expenditure Index (Index ",gsub("\\y","",baseyear),"=100)")
  
  x <- mbind(x1, x2)
  
  return(x)
}