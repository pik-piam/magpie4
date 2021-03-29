#' @title reportExpenditureFoodIndex
#' @description reports food expenditure index (corrected for emission costs)
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param baseyear baseyear of the price index
#' @param basketyear year of reference food basket

#' @return Food expenditure index as MAgPIE object 
#' @author Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceFoodIndex(gdx)
#'   }
#' 

reportExpenditureFoodIndex <- function(gdx, baseyear="y2010", basketyear="y1995") {

  # corrected food price index (food expenditure index) all food products
  x <- expenditureIndexFood(gdx, level="regglo", basketyear=basketyear, baseyear=baseyear, round=TRUE)
  getNames(x) <- paste0("Prices|Food Expenditure Index corrected for ghg costs (Index ",gsub("\\y","",baseyear),"=100)")
  
  return(x)
}