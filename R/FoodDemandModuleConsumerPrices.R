#' @title FoodDemandModuleConsumerPrices
#' @description Calculates food prices that enter demand module
#'
#' @export
#'
#' @param gdx GDX file
#' @param level reg or iso
#' @param valueAdded whether to add the value-added 
#' marketing margin to the total expenditures
#'
#'
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- FoodDemandModuleConsumerPrices(gdx)
#'   }
#'

FoodDemandModuleConsumerPrices<-function(gdx, level="iso", valueAdded = FALSE){
  

  price = lastIter(gdx,"p15_prices_kcal")

  if ((!identical(valueAdded, FALSE)) ) {
  
    if (valueAdded == "valueAddedFAH") {
      margin <- suppressWarnings((readGDX(gdx, "p15_marketing_margin_fah_kcal")))
    } else if (valueAdded == "valueAddedFAFH") {
      margin <- suppressWarnings((readGDX(gdx, "p15_marketing_margin_fafh_kcal")))
    } else {
      warning("Food value added is either valueAddedFAH or valueAddedFAFH")}
    p <- price + margin 
     } 

  if (level=="reg"){
    out = gdxAggregate(gdx,price,weight=price*0+1,to="reg",absolute=FALSE)
  } else if (level=="iso"){
    out=price
  } else (stop("undefined level"))




  return(out)
}
