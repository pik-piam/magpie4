#' @title reportCostCapitalStocks
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
#'     x <- reportCostCapitalStocks(gdx)
#'   }
#' @importFrom magclass getNames

reportCostCapitalStocks<-function(gdx){
  
  #Capital stocks used in croland per region 
  x <- CostCapital(gdx,level = "regglo",type="stocks")
  getNames(x)<-"Costs|Capital Stocks"
  
  
  getNames(x) <- paste0(getNames(x)," (million US$05)")
  
  return(x)
  
  
}
