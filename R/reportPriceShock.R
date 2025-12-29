#' @title reportPriceShock
#' @description Reports the change in consumption and expenditure due to higher or lower food prices
#' 
#' @export
#'
#' @param gdx GDX file
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceShock(gdx)
#'   }
#' 
#'
#' @section Price shock variables:
#' Name | Unit | Meta
#' ---|---|---
#' Food Supply\|Calorie Supply\|Price Induced Change\|Absolute\|Total Calories | kcal/cap/day | Absolute total
#' Food Supply\|Calorie Supply\|Price Induced Change\|Absolute\|Livestock Calories | kcal/cap/day | Livestock
#' Food Supply\|Calorie Supply\|Price Induced Change\|Relative\|Total Calories | kcal/kcal | Relative total
#' Household Expenditure\|Food\|Price Induced Change\|Absolute\|Food Expenditure | USD/cap | Absolute expense
#' Household Expenditure\|Food\|Price Induced Change\|Relative\|Food Expenditure | USD/USD | Relative expense
#' @md


reportPriceShock<-function(gdx){
  if(!is.null(readGDX(gdx,"p15_delta_kcal_pc", react="silent"))){
    #warning("calibrated should be TRUE")
    calibrated <- FALSE
    kap<-findset("kap")
    before<-Kcal(gdx,level = "regglo",after_shock = FALSE,product_aggr = FALSE,calibrated=calibrated)
    after<-Kcal(gdx,level="regglo",after_shock = TRUE,product_aggr = FALSE,calibrated=calibrated)
    shock_total_relative<-dimSums(after,dim=3.1)/dimSums(before,dim=3.1)
    shock_livestock_relative<-dimSums(after[,,kap],dim=3.1)/dimSums(before[,,kap],dim=3.1)
    shock_others_relative<-dimSums(after[,,"others"],dim=3.1)/dimSums(before[,,"others"],dim=3.1)
    
    shock_total_abs<-dimSums(after,dim=3.1)-dimSums(before,dim=3.1)
    shock_livestock_abs<-dimSums(after[,,kap],dim=3.1)-dimSums(before[,,kap],dim=3.1)
    shock_others_abs<-dimSums(after[,,"others"],dim=3.1)-dimSums(before[,,"others"],dim=3.1)
    
    shock_expenditure_abs=FoodExpenditure(gdx = gdx,level="regglo",after_shock = TRUE)-FoodExpenditure(gdx = gdx,level="regglo",after_shock = FALSE)
    shock_expenditure_relative=FoodExpenditure(gdx = gdx,level="regglo",after_shock = TRUE)/FoodExpenditure(gdx = gdx,level="regglo",after_shock = FALSE)
    
    #shock_hunger_abs=Hunger(gdx = gdx,level="regglo",after_shock = TRUE)-Hunger(gdx = gdx,level="regglo",after_shock = FALSE)
    #shock_hunger_relative=Hunger(gdx = gdx,level="regglo",after_shock = TRUE,share=TRUE)-Hunger(gdx = gdx,level="regglo",after_shock = FALSE,share=TRUE)
    
    getNames(shock_total_abs)="Food Supply|Calorie Supply|Price Induced Change|Absolute|Total Calories (kcal/capita/day)"
    getNames(shock_livestock_abs)="Food Supply|Calorie Supply|Price Induced Change|Absolute|Livestock Calories (kcal/cap/day)"
    getNames(shock_others_abs)="Food Supply|Calorie Supply|Price Induced Change|Absolute|Vegetable, Fruits and Nuts Calories (kcal/cap/day)"
    
    getNames(shock_total_relative)="Food Supply|Calorie Supply|Price Induced Change|Relative|Total Calories (kcal/kcal)"
    getNames(shock_livestock_relative)="Food Supply|Calorie Supply|Price Induced Change|Relative|Livestock Calories (kcal/kcal)"
    getNames(shock_others_relative)="Food Supply|Calorie Supply|Price Induced Change|Relative|Vegetable, Fruits and Nuts Calories (kcal/kcal)"
    
    getNames(shock_expenditure_abs)="Household Expenditure|Food|Price Induced Change|Absolute|Food Expenditure (USD/capita)"
    getNames(shock_expenditure_relative)="Household Expenditure|Food|Price Induced Change|Relative|Food Expenditure (USD/USD)"
  
    #getNames(shock_hunger_abs)="Food Supply|Calorie Supply|Price Induced Change|Undernourished (Mio People)"
    #getNames(shock_hunger_relative)="Food Supply|Calorie Supply|Price Induced Change|Share of population undernourished (percentage points)"
    
    out<-mbind(shock_total_abs,
               shock_livestock_abs,
               shock_others_abs,
               shock_total_relative,
               shock_livestock_relative,
               shock_others_relative,
               shock_expenditure_abs,
               shock_expenditure_relative,
               #shock_hunger_abs,
               #shock_hunger_relative
               )
    
    #delete empty categories
    out<-out[,,getNames(out)[which(dimSums(out,dim=c(1,2))!=0)]]
    return(out)
  } else {
    return(NULL)
  }
}
