#' @title Kcal
#' @description Calculates the per-capita kcal consumption from the food demand model
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not (boolean)
#' @param after_shock FALSE is using the exogenous real income and the prices before a shock, TRUE is using the endogenous real income that takes into account food price change on real income
#' @param calibrated if FALSE, the true regression outputs are used, if TRUE the values calibrated to the start years are used
#' @param magpie_input if TRUE, the per-capita kcal consumption values finally entering MAgPIE as input are used. In cases where exogenous diet scnearios (e.g. EAT Lancet diets) are used,
#' these values can diverge from the (calibrated) regression outputs. This setting can only be activated if argumanets "calibrated" and "after_shock" are set to TRUE.
#' @param attributes unit: kilocalories per day ("kcal"), g protein per day ("protein"). Mt reactive nitrogen ("nr").
#' @param per_capita per capita or aggregated for the population 
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @details Demand definitions are equivalent to FAO Food supply categories
#' @return calories as MAgPIE object (unit depends on per_capita: kcal/cap/day (TRUE), kcal/day (FALSE))
#' @author Benjamin Leon Bodirsky, Isabelle Weindl
#' @importFrom gdx readGDX
#' @importFrom magpiesets findset
#' @importFrom magclass mbind getYears getNames<- collapseNames dimSums
#' @examples
#' 
#'   \dontrun{
#'     x <- Kcal(gdx)
#'   }
#' 

Kcal <- function(gdx, 
                 file=NULL, 
                 level="reg", 
                 products="kfo", 
                 product_aggr=TRUE, 
                 after_shock=TRUE, 
                 calibrated=TRUE,
                 magpie_input=TRUE,
                 attributes="kcal",
                 per_capita=TRUE,
                 spamfiledirectory=""){

  
  # retrieve the right data
  if (calibrated==FALSE){
    
    if (after_shock==TRUE){
      out<-readGDX(gdx=gdx,"ov15_kcal_regr",select=list(type="level"))  
    } else if (after_shock==FALSE){
      out<-readGDX(gdx=gdx,"o15_kcal_regr_initial")
    } else {stop("after_shock has to be binary")}
      
  } else {
    if (after_shock==TRUE){
      if (magpie_input==FALSE){
        out<-readGDX(gdx=gdx,"p15_kcal_pc_iso")
      } else {
        kcal_pc_calibrated<-readGDX(gdx=gdx,"p15_kcal_pc_calibrated")
        balance_flow<-readGDX(gdx=gdx,"p15_balanceflow_kcal")
        #revert MAgPIE-internal calibration: While the food demand model estimates demand for all countries, 
        #FAOSTAT only covers a subset. To match FAOSTAT totals, the food demand of countries not included
        #in FAOSTAT is calibrated to zero in MAgPIE.  
        out<-kcal_pc_calibrated - balance_flow
      }
    } else if (after_shock==FALSE){
      out<-readGDX(gdx=gdx,"p15_kcal_pc_initial_iso")
    } else {stop("after_shock has to be binary")}
  }
  
  out<-gdxAggregate(gdx = gdx,x = out,weight = 'population',to = level,absolute = FALSE,spamfiledirectory = spamfiledirectory)
  
  
  if (identical("kall",products)){
    missing=setdiff(readGDX(gdx,"kall"),getNames(out))
    out<-add_columns(out,dim = 3.1,addnm = missing)
    out[,,missing]=0
  } else if (!all(products%in%findset("kall"))){
      products<-findset(products)
      out<-out[,,products]
  } else {
    out<-out[,,products]
  }
    
  
  if(product_aggr){out<-dimSums(out,dim=3.1)}
  
  if (per_capita) {
    out=out
  } else {
    pop=population(gdx=gdx,level=level)
    out=out*pop
  }
  
  if(any(attributes!="kcal")){
    att=att2=readGDX(gdx=gdx,"f15_nutrition_attributes")[,getYears(out),products]
    att2<-att2[,,"protein"]*365/6.25/1000000
    getNames(att2,dim = 2)<-"nr"
    att<-mbind(att,att2)
    out<-out/collapseNames(att[,,"kcal"],collapsedim = 2)*att[,,attributes]
    out[is.na(out)]<-0
  }
  
  out(out,file)
}