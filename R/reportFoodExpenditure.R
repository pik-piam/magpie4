#' @title reportFoodExpenditure
#' @description reports per-capita calories food supply (including household waste)
#' 
#' @importFrom magpiesets reporthelper 
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return per-capita calories as MAgPIE object (kcal/cap/day)
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportFoodExpenditure(gdx)
#'   }
#' 

reportFoodExpenditure<-function(gdx,detail=FALSE){
  out<-FoodExpenditure(gdx,level = "regglo",products="kall",product_aggr = FALSE)
  out<-reporthelper(x=out,level_zero_name = "Household Expenditure|Food|Expenditure",detail = detail,partly = TRUE)  
  getNames(out) <- paste(getNames(out),"(USD/capita)",sep=" ")
  
  out2<-FoodExpenditureShare(gdx,level="regglo",products = "kfo",product_aggr = TRUE)
  getNames(out2) <- "Household Expenditure|Food|Food Expenditure Share (USD05/USD05)"
  
  out<-mbind(out,out2)
  #delete empty categories
  out<-out[,,getNames(out)[which(dimSums(out,dim=c(1,2))!=0)]]
  return(out)
}
