#' @title IntakeDetailedProtein
#' @description Calculates detailed per-capita (protein in grams) intake from magpie results at regional level
#' @export
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @return Product disaggregated Protein intake as MAgPIE object at regional level (unit: grams/cap/day)
#' @author Vartika Singh, Isabelle Weindl
#' @importFrom gdx readGDX
#' @importFrom magclass dimSums
#' @examples
#' 
#'   \dontrun{
#'     x <- IntakeDetailedProtein(gdx)
#'   }
#' 

IntakeDetailedProtein <- function(gdx,file=NULL){
  
  # intake of different foods has to be back-calculated from food calorie availability and assumptions on food waste:  
      kcal_intake <- Intake(gdx,level="reg",calibrated=TRUE,pregnancy=FALSE,per_capita=TRUE,
                            age=FALSE,sex=FALSE,bmi_groups=FALSE)
      pregnancy_iso <- readGDX(gdx,"i15_kcal_pregnancy")
      pregnancy<-gdxAggregate(gdx,x = pregnancy_iso,weight = 'population',to = "reg",absolute = TRUE)
      
      pop<-population(gdx, level="reg",age = FALSE,sex=FALSE,bmi_groups = FALSE)
      kcal_intake <- kcal_intake + pregnancy/pop
      
      kcal_avail_detailed <- readGDX(gdx,"p15_kcal_pc_calibrated")
      demand2intake   <- readGDX(gdx,"p15_demand2intake_ratio_scen")
      
      FAO_waste <- readGDX(gdx,"f15_overcons_FAOwaste")
      FAO_fsupply_calib <- readGDX(gdx,"f15_calib_fsupply")
      demand2intake_ref <- readGDX(gdx,"p15_demand2intake_ratio_ref")
      Mag_waste_growth <- demand2intake/demand2intake_ref
        
      intake_scen <- kcal_avail_detailed/(FAO_fsupply_calib*FAO_waste)*(1/Mag_waste_growth) 
      intake_scen <- intake_scen*(kcal_intake/dimSums(intake_scen,dim=3))
        
    
      att=readGDX(gdx=gdx,"f15_nutrition_attributes")[,getYears(intake_scen),getNames(intake_scen,dim=1)]
      intake_scen<-intake_scen/collapseNames(att[,,"kcal"],collapsedim = 2)*att[,,"protein"]
  
  out(intake_scen,file)
  
}
  

  
  
