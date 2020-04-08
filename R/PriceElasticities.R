#' @title PriceElasticities
#' @description Calculates the physical elasticity for food demand
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)
#' @param calibrated if FALSE, the true regression outputs are used, if TRUE the values calibrated to the start years are used
#' @param products set of the products for which the elasticity should be estimated. Please note that this stills remains an elasticity relative to total food expenditure. So its the change in consumption of one good when the prices of all products change according to the scenario.
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
                          products="kfo"){
  
  kcal_before<-Kcal(gdx, level="iso", products=products, 
             product_aggr=FALSE, 
             after_shock=FALSE, 
             calibrated=calibrated,
             attributes="kcal",
             per_capita=TRUE,
             magpie_input=FALSE)
  
  kcal_after<-Kcal(gdx, level="iso", products=products, 
                    product_aggr=FALSE, 
                    after_shock=TRUE, 
                    calibrated=calibrated,
                    attributes="kcal",
                    per_capita=TRUE,
                    magpie_input=FALSE)
  
  weight<-Kcal(gdx, level="iso", products="kfo", 
                   product_aggr=FALSE, 
                   after_shock=TRUE, 
                   calibrated=calibrated,
                   attributes="kcal",
                   per_capita=TRUE,
                   magpie_input=FALSE)
  
  caloriechange=(dimSums(kcal_after)/dimSums(kcal_before)-1)
  
  expenditure_change = (dimSums(readGDX(gdx,"p15_prices_kcal")*weight,dim=3)
                        /dimSums(readGDX(gdx,"i15_prices_initial_kcal")*weight,dim=3))-1

  elasticity=caloriechange/expenditure_change
  elasticity=round(elasticity,5)
  
  out<-gdxAggregate(gdx = gdx,x = elasticity,weight = 'population',to = level,absolute = FALSE)
  
  out(out,file)
}
