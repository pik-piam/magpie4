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
  
  allprice = readGDX(gdx,"p15_prices_kcal")
  if(length(fulldim(allprice)[[1]])==4){
    price=collapseNames(allprice[,,"iter1"])
    
    lastiter=readGDX(gdx,"iter15")[readGDX(gdx,"p15_iteration_counter")-1]
    for(t in 1:nyears(allprice)) {
      price[,t,] = allprice[,t,lastiter[t]]
    }
  } else { # downwards compatbility
    price=allprice
  }
  
  return(price)
}