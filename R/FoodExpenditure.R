#' @title FoodExpenditure
#' @description Calculates the food expenditure in USD per year
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level spatial aggregation. can be "iso","reg","regglo","glo"
#' @param after_shock FALSE is using the exogenous real income and the prices before a shock, TRUE is using the endogeenous real income that takes into account food price change on real income, "after_price_before_demand" takes into account price changes on real income, but assumes no demand adjustment
#' @param products selected products or sets of products
#' @param product_aggr if true, aggregation over products
#' @param per_capita per capita or total population
#' 
#' @return magpie object with per capita consumption
#' @author Benjamin Leon Bodirsky
#' @importFrom magclass colSums mbind add_columns dimSums getNames
#' @importFrom gdx readGDX
#' @importFrom magpiesets findset
#' @examples
#' 
#'   \dontrun{
#'     x <- FoodExpenditure(gdx)
#'   }
#' 

FoodExpenditure<-function(gdx,level="reg",after_shock=TRUE,products="kfo",product_aggr=TRUE, per_capita=TRUE){

  
  if(after_shock==TRUE){
    price = FoodDemandModuleConsumerPrices(gdx)
    
    value = price *
      Kcal(gdx=gdx,
           level="iso",
           calibrated=TRUE,
           after_shock = TRUE,
           products="kfo",
           product_aggr = FALSE,
           per_capita=FALSE
           )
    
    out<-gdxAggregate(
      gdx=gdx,
      x=value,
      weight="Kcal",
      to=level,
      absolute=TRUE,
      
      #arguments of weight
      calibrated=TRUE,
      after_shock = TRUE,
      products="kfo",
      product_aggr = FALSE,
      per_capita=FALSE
      
    )

  } else if (after_shock=="after_price_before_demand") {
    value = FoodDemandModuleConsumerPrices(gdx) *    #prices with shock
      Kcal(gdx=gdx,
           level="iso",
           calibrated=TRUE,
           after_shock = FALSE,  #demand without shock
           products="kfo",
           product_aggr = FALSE,
           per_capita=FALSE
           
      )
           
                
    out<-gdxAggregate(
      gdx=gdx,
      x=value,
      weight="Kcal",
      to=level,
      absolute=TRUE,
      
      
      #arguments of weight  
      calibrated=TRUE,
      after_shock = FALSE,
      products="kfo",
      product_aggr = FALSE,
      per_capita=FALSE
      
    )
  } else if (after_shock==FALSE){
    value = readGDX(gdx,"i15_prices_initial_kcal") *
      Kcal(gdx=gdx,
           level="iso",
           calibrated=TRUE,
           after_shock = FALSE, 
           products="kfo",
           product_aggr = FALSE,
           per_capita=FALSE
           
      )
    
    out<-gdxAggregate(
      gdx=gdx,
      x=value,
      weight="Kcal",
      to=level,
      absolute=TRUE,
      
      #arguments of weight  
      calibrated=TRUE,
      after_shock = FALSE,
      products="kfo",
      product_aggr = FALSE,
      per_capita=FALSE
      
    )
  } else {stop("after_shock has to be binary")}
  
  if (per_capita==TRUE){
    pop<-population(gdx,level=level)
    out=out/pop
    out[is.nan(out)]<-0
  } 
  
  if(products=="kall"){
    missing<-setdiff(readGDX(gdx,"kall"),getNames(out))
    out<-add_columns(out,addnm = missing,dim = 3.1)
    out[,,missing]<-0
    products<-findset(products)
  } else if (!products%in%getNames(out)){
    products<-findset(products)
  }
  
  out<-out[,,products]*365  ## transform into dollar per year
  
  if(product_aggr){out <- dimSums(out,dim=3.1)}
  
  return(out)
}