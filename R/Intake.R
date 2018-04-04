#' @title Intake
#' @description Calculates the per-capita kcal intake from the food demand model
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)
#' @param calibrated if FALSE, the true regression outputs are used, if TRUE the values calibrated to the start years are used
#' @param pregnancy if TRUE, adding the intake requriements for lactation and pregnancy
#' @param per_capita per capita or aggregated for the population 
#' @param age_groups if FALSE age_groups and sex is aggregated
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @details Demand definitions are equivalent to FAO Food supply categories
#' @return calories as MAgPIE object (unit depends on per_capita: kcal/cap/day (TRUE), kcal/day (FALSE))
#' @author Benjamin Leon Bodirsky
#' @importFrom gdx readGDX
#' @importFrom magpiesets findset
#' @importFrom magclass mbind getYears getNames<- collapseNames dimSums
#' @examples
#' 
#'   \dontrun{
#'     x <- Kcal(gdx)
#'   }
#' 

Intake <- function(gdx, 
                 file=NULL, 
                 level="reg", 
                 calibrated=TRUE,
                 pregnancy=TRUE,
                 per_capita=TRUE,
                 age_groups=FALSE,
                 spamfiledirectory=""){
  
  out <- collapseNames(readGDX(gdx,"ov15_kcal_intake_regression")[,,"level"],collapsedim = "type")
  
  if(calibrated==TRUE){
    out=out+readGDX(gdx,"p15_intake_balanceflow")
  }
  
  if(pregnancy==FALSE){
    preg <- readGDX(gdx,"p15_kcal_pregnancy")
    req  <- readGDX(gdx,"p15_kcal_requirement")
    out=out/(preg+req)*req
  }
  
  if(age_groups==FALSE){
    pop<-population(gdx, age_groups = TRUE,sex=TRUE,level="iso")
    out=dimSums(pop*out,dim=c("sex","age_group"))/dimSums(pop,dim=c("sex","age_group"))
  }
  
  
  out<-gdxAggregate(gdx = gdx,x = out,weight = 'population',to = level,absolute = FALSE,spamfiledirectory = spamfiledirectory)
  
  if (per_capita) {
    out=out
  } else {
    pop=population(gdx=gdx,level=level)
    out=out*pop
  }
  
  out(out,file)
}