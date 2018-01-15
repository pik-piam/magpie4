#' @title PriceElasticities
#' @description Calculates the physical elasticity for food demand
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)
#' @param calibrated if FALSE, the true regression outputs are used, if TRUE the values calibrated to the start years are used
#' @param product_aggr if TRUE, elasticity over all products is estimated
#' 
#' @return magpie object with the livestock share in a region or country. Unit is dimensionsless, but value depends on chosen attribute
#' @author Benjamin Bodirsky
#' @importFrom gdx readGDX out
#' @importFrom magclass getYears
#' @examples
#' 
#'   \dontrun{
#'     x <- PriceElasticities(gdx)
#'   }
#' 


PriceElasticities<- function(gdx, 
                          file=NULL, 
                          level="reg", 
                          calibrated=TRUE,
                          product_aggr=FALSE){
  
  kcal_before<-Kcal(gdx, level=level, products="kfo", 
             product_aggr=product_aggr, 
             after_shock=FALSE, 
             calibrated=calibrated,
             attributes="kcal",
             per_capita=TRUE)
  
  kcal_after<-Kcal(gdx, level=level, products="kfo", 
                    product_aggr=product_aggr, 
                    after_shock=TRUE, 
                    calibrated=calibrated,
                    attributes="kcal",
                    per_capita=TRUE)
  
  pricechange=readGDX(gdx,"f15_price_index")
  
  elasticity=(kcal_after/kcal_before-1)/(pricechange[,getYears(kcal_after),]-1)
  
  
  
  out(elasticity,file)
}
