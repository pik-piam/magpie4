#' @title IntakeDetailed
#' @description Calculates detailed or aggregated per-capita kcal intake including exogenous scenarios
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param product_aggr aggregate over products or not (boolean)
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @details Calculation of kcal food intake is possible for both exogenous diet scenarios and endogenous estimation from food demand model 
#' @return Calories as MAgPIE object (unit: kcal/cap/day)
#' @author Isabelle Weindl
#' @importFrom gdx readGDX
#' @importFrom magclass dimSums
#' @examples
#' 
#'   \dontrun{
#'     x <- IntakeDetailed(gdx)
#'   }
#' 

IntakeDetailed <- function(gdx, 
                   file=NULL, 
                   level="reg", 
                   product_aggr=FALSE,
                   spamfiledirectory=""
                   ){

  # retrieve data correct data
  exo_diet_switch <- readGDX(gdx=gdx,"s15_exo_diet")  
  
  # if simualtions are based on exogenous diets, intake can be directly taken from definition of model inputs:
  if (exo_diet_switch == 1) {
    if (product_aggr == FALSE) {
      intake_scen <- readGDX(gdx,"i15_intake_detailed_scen_target")
    } else {
      intake_scen <- readGDX(gdx,"i15_intake_scen_target")
    }
  }  
    
  
  # if simualtions are based on endogenous diets, intake of different foods has to be back-calculated from 
  # food calorie availability and assumptions on food waste:  
  if (exo_diet_switch != 1) {
    
    kcal_avail_detailed <- readGDX(gdx,"p15_kcal_pc_calibrated")
    kcal_avail <- dimSums(kcal_avail_detailed,dim=3)
    demand2intake   <- readGDX(gdx,"p15_demand2intake_ratio_scen")
    kcal_intake <- kcal_avail/demand2intake
    
    if (product_aggr == FALSE) {
     
      FAO_waste <- readGDX(gdx,"f15_overcons_FAOwaste")
      FAO_fsupply_calib <- readGDX(gdx,"f15_calib_fsupply")
      Mag_waste_growth <- readGDX(gdx,"p15_foodwaste_growth")
      
      intake_scen <- kcal_avail_detailed/(FAO_fsupply_calib*FAO_waste)*(1/Mag_waste_growth) 
      intake_scen <- intake_scen*(kcal_intake/dimSums(intake_scen,dim=3))
      
      
    } else {
      intake_scen <- kcal_intake
    }
  }  
  
  out<-gdxAggregate(gdx = gdx,x = intake_scen,weight = 'population',to = level,absolute = FALSE,spamfiledirectory = spamfiledirectory)
  
  out(out,file)
}