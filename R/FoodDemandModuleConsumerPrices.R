#' @title FoodDemandModuleConsumerPrices
#' @description Calculates food prices that enter demand module
#'
#' @export
#'
#' @param gdx GDX file
#'
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @importFrom gdx readGDX
#' @examples
#'
#'   \dontrun{
#'     x <- FoodDemandModuleConsumerPrices(gdx)
#'   }
#'

FoodDemandModuleConsumerPrices<-function(gdx){

  price = lastIter(gdx,"p15_prices_kcal")

  return(price)
}
