#' @title FoodDemandModuleConsumerPrices
#' @description Calculates food prices that enter demand module
#'
#' @export
#'
#' @param gdx GDX file
#' @param level reg or iso
#'
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- FoodDemandModuleConsumerPrices(gdx)
#'   }
#'

FoodDemandModuleConsumerPrices<-function(gdx,level="iso"){

  price = lastIter(gdx,"p15_prices_kcal")

  if (level=="reg"){
    out = gdxAggregate(gdx,price,weight=price*0+1,to="reg",absolute=FALSE)
  } else if (level=="iso"){
    out=price
  } else (stop("undefined level"))

  return(out)
}
