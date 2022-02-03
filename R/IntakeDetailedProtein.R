#' @title IntakeDetailedProtein
#' @description Calculates food-specific per-capita protein intake from magpie results in grams. 
#' @export
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param target_diet returns target diet of dietary transformation in case of exogenous diet scenarios (boolean); 
#' in case of endogenous diets, no target diet is defined and the function returns an object filled with 0.
#' @param magpie_input Available modes are "auto" (default), TRUE or FALSE. 
#' This setting is only activate if argument "target_diet" is set to FALSE and else ignored. 
#' If set as TRUE, the per-capita kcal intake values finally entering MAgPIE as input are used, which drive the behaviour of 
#' the MAgPIE model. In cases where exogenous diet scenarios (e.g. EAT Lancet diets) are simulated, these input values can diverge 
#' from the (calibrated) regression outputs from the food demand model. 
#' If set as FALSE, the per-capita kcal intake values as calculated in the food demand model are used, which might be 
#' overwritten in the MAgPIE simulation in the case of exogenous diet scenarios (e.g. EAT Lancet diets). 
#' The default setting "auto" detects automatically, if an exogenous scenario for per-capita kcal intake is simulated by MAgPIE,
#' and uses the respective settings: 1) magpie input in case of exogenous scenarios and 2) estimates from the food demand model in
#' case of endogenous scenarios.
#' @param product_aggr aggregate over products or not (boolean)
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param file a file name the output should be written to using write.magpie
#' @return Protein intake as MAgPIE object (unit: grams/cap/day)
#' @author Vartika Singh, Isabelle Weindl
#' @importFrom gdx readGDX
#' @importFrom magclass dimSums
#' @examples
#' 
#'   \dontrun{
#'     x <- IntakeDetailedProtein(gdx)
#'   }
#' 

IntakeDetailedProtein <- function(gdx, file=NULL, level="reg", target_diet=FALSE,magpie_input="auto",product_aggr=FALSE, dir="."){
  
  #Obtains intake calorific information at regional level, product disaggregated
  intake_scen <- IntakeDetailed(gdx, level="reg",target_diet=target_diet, magpie_input=magpie_input, product_aggr=FALSE)
  
  #Extracts information on protein from food groups
  att=readGDX(gdx=gdx,"f15_nutrition_attributes")[,getYears(intake_scen),getNames(intake_scen,dim=1)]
  intake_scen <- intake_scen / collapseNames(att[,,"kcal"]) * collapseNames(att[,,"protein"])
 
  if(product_aggr){intake_scen<-dimSums(intake_scen,dim=3)}
      
  #Aggregates to level as selected in the argument
  out<-gdxAggregate(gdx = gdx,x = intake_scen,weight = 'population',to = level,absolute = FALSE,dir = dir)
  
  out(out,file)
  
}
  

  
  
