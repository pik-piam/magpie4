#' @title Intake
#' @description Calculates the per-capita kcal intake from the food demand model
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)
#' @param calibrated if FALSE, the true regression outputs are used, if TRUE the values calibrated to the start years are used
#' @param pregnancy if TRUE, adding the intake requirements for lactation and pregnancy
#' @param per_capita per capita or aggregated for the population
#' @param age if FALSE age and sex is aggregated
#' @param sex if TRUE, data is provided by sex
#' @param bmi_groups if TRUE data is proided by BMI group
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @details Demand definitions are equivalent to FAO Food supply categories
#' @return calories as MAgPIE object (unit depends on per_capita: kcal/cap/day (TRUE), kcal/day (FALSE))
#' @author Benjamin Leon Bodirsky
#' @importFrom magpiesets findset
#' @importFrom magclass mbind getYears getNames<- collapseNames dimSums
#' @examples
#'
#'   \dontrun{
#'     x <- Intake(gdx)
#'   }
#'

Intake <- function(gdx,
                 file=NULL,
                 level="reg",
                 calibrated=TRUE,
                 pregnancy=FALSE,
                 per_capita=TRUE,
                 age=FALSE,
                 sex=FALSE,
                 bmi_groups=FALSE,
                 dir=".",
                 spamfiledirectory=""){


  dir <- getDirectory(dir,spamfiledirectory)

  bmi_shr=anthropometrics(gdx = gdx,indicator = "bmi_shr",age = TRUE,sex = TRUE,bmi_groups = TRUE,calibrated = calibrated, level="iso")
  pop<-population(gdx, age = TRUE,sex=TRUE,bmi_groups = FALSE, level="iso")
  intake = readGDX(gdx,"p15_intake")
  weight=pop*bmi_shr
  out=weight*intake

  if(age==FALSE){
    out<-dimSums(out,dim="age")
  } else if (age!=TRUE) {
    out<-out[,,age]
    weight<-weight[,,age]
    out<-dimSums(out,dim="age")
    weight<-dimSums(weight,dim="age")
  }

  if(sex==FALSE){
    out<-dimSums(out,dim="sex")
  } else if (sex!=TRUE) {
    out<-out[,,sex]
    weight<-weight[,,sex]
    out<-dimSums(out,dim="sex")
    weight<-dimSums(weight,dim="sex")
  }

  if(bmi_groups==FALSE){
    out<-dimSums(out,dim="bmi_group15")
  } else if (bmi_groups!=TRUE) {
    out<-out[,,bmi_groups]
    weight<-weight[,,bmi_groups]
    out<-dimSums(out,dim="bmi_group15")
    weight<-dimSums(weight,dim="bmi_group15")
  }

  out<-gdxAggregate(gdx = gdx,x = out,weight = 'population',to = level,absolute = TRUE,dir = dir)

  if (per_capita) {
    pop=population(gdx=gdx,level=level,sex=sex,age = age,bmi_groups = bmi_groups)
    out=out/pop
  } else {
    out=out
  }

  if(pregnancy==TRUE){
    if(sex!=FALSE|age!=FALSE|bmi_groups!=FALSE|per_capita!=FALSE) {stop("pregnancy only works for aggregated results over age groups and gender")}
    pregnancy=readGDX(gdx,"i15_kcal_pregnancy")
    out<-out+pregnancy
  }

  out(out,file)
}
