#' @title IntakeDetailed
#' @description Calculates detailed or aggregated per-capita kcal intake including exogenous scenarios
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
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
#' @param spamfiledirectory deprecated. please use \code{dir} instead
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
                           target_diet=FALSE,
                           magpie_input="auto",
                           product_aggr=FALSE,
                           dir=".",
                           spamfiledirectory=""
){

  dir <- getDirectory(dir,spamfiledirectory)

  p15_intake_detail = readGDX(gdx,"p15_intake_detail",react="silent")
  if (length(p15_intake_detail)>0){ # New Realization
    if (target_diet==TRUE){stop("This setting is not yet implemented for new realization")}
    if (magpie_input==FALSE){stop("This setting is not yet implemented for new realization")}
    intake_scen=p15_intake_detail
    agg=dimSums(intake_scen)
    # Test
    if(abs(sum(agg)-sum(readGDX(gdx,"p15_intake_total"))>0.001)){warning("intake inconsistent with intake_detail")}
    if(product_aggr){intake_scen <- agg}
  } else {  # Older realization. To be discontinued.

    if(magpie_input=="auto") {
      exo_diet <- readGDX(gdx=gdx,"s15_exo_diet")
      magpie_input=FALSE
      if (!is.null(exo_diet)){
        if(exo_diet>0){
          magpie_input=TRUE
        }
      }
    }

    # target intake for exogenous diet scenarios can directly be taken from model inputs:
    if (target_diet == TRUE) {
      if (product_aggr == FALSE) {
        intake_scen <- readGDX(gdx,"i15_intake_detailed_scen_target")
      } else {
        intake_scen <- readGDX(gdx,"i15_intake_scen_target")
      }
    }


    # intake of different foods has to be back-calculated from food calorie availability and assumptions on food waste:
    if (target_diet == FALSE) {

      kcal_intake <- Intake(gdx,level="reg",calibrated=TRUE,pregnancy=FALSE,per_capita=TRUE,
                            age=FALSE,sex=FALSE,bmi_groups=FALSE)
      pregnancy_iso <- readGDX(gdx,"i15_kcal_pregnancy")
      pregnancy<-gdxAggregate(gdx,x = pregnancy_iso,weight = 'population',to = "reg",absolute = TRUE)

      pop<-population(gdx, level="reg",age = FALSE,sex=FALSE,bmi_groups = FALSE)
      kcal_intake <- kcal_intake + pregnancy/pop

      if (magpie_input==TRUE){
        intake_target <- readGDX(gdx,"i15_intake_scen_target")
        fader <- readGDX(gdx,"i15_exo_foodscen_fader")
        kcal_intake <- kcal_intake*(1-fader) + intake_target*fader
      }

      if (product_aggr == FALSE) {

        kcal_avail_detailed <- readGDX(gdx,"p15_kcal_pc_calibrated")
        demand2intake   <- readGDX(gdx,"p15_demand2intake_ratio_scen")

        FAO_waste <- readGDX(gdx,"f15_overcons_FAOwaste")
        FAO_fsupply_calib <- readGDX(gdx,"f15_calib_fsupply")
        demand2intake_ref <- readGDX(gdx,"p15_demand2intake_ratio_ref")
        Mag_waste_growth <- demand2intake/demand2intake_ref

        intake_scen <- kcal_avail_detailed/(FAO_fsupply_calib*FAO_waste)*(1/Mag_waste_growth)
        intake_scen <- intake_scen*(kcal_intake/dimSums(intake_scen,dim=3))

      } else {
        intake_scen <- kcal_intake
      }
    }
  }
  out<-gdxAggregate(gdx = gdx,x = intake_scen,weight = 'population',to = level,absolute = FALSE,dir = dir)
  out(out,file)
}
