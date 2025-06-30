#' @title FoodExpenditureShare
#' @description Calculates the livestock share from the food demand model
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level spatial aggregation. can be "iso","reg","regglo","glo"
#' @param products selected products or sets of products
#' @param product_aggr if true, aggregation over products
#' @param after_shock FALSE is using the exogenous real income and the prices before a shock, TRUE is using the endogeenous real income that takes into account food price change on real income
#' @param valueAdded TRUE to include post farmgate value added in the expenditure share
#' @return magpie object with per capita consumption
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- FoodExpenditureShare(gdx)
#'   }
#' 



FoodExpenditureShare<-function(gdx,level="reg",after_shock=TRUE,products="kfo",product_aggr=TRUE, valueAdded = FALSE){
  foodexpenditure<-FoodExpenditure(gdx, level = level, after_shock = after_shock, products = products,
                                   product_aggr = product_aggr, valueAdded = valueAdded)
  income<-income(gdx,level=level)
  share=foodexpenditure/income
  return(share)
}
