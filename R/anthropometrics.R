#' @title anthropometrics
#' @description Calculates anthropometic indicators from the food demand model
#' @param gdx GDX file
#' @param indicator bodyheight, bodyweight, bodyweight_healthy, BMI(Body Mass Index) or PAL (physical activity level)
#' @param level Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)
#' @param age if TRUE, demand is scaled down to age-groups and sex using food requirements
#' @param sex if FALSE, female and male are aggregated, if sex, results are divided into males and females
#' @param bmi_groups if TRUE, data is provided by BMI group
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @param final final results or preliminary results (the latter are the ones magpie uses for optimization before last iteration with demand model)
#' @param file a file name the output should be written to using write.magpie
#' @param calibrated if TRUE, uses the calibrated intake estimates for bodyweight estimation
#' @details Demand definitions are equivalent to FAO Food supply categories
#' @return bodyweight (kg), bodyheight (cm), BMI or PAL as magpie objects
#' @author Benjamin Leon Bodirsky
#' @importFrom gdx readGDX
#' @importFrom magpiesets findset
#' @importFrom magclass collapseNames dimSums
#' @export
#' @examples
#'
#'   \dontrun{
#'     x <- anthropometrics(gdx)
#'   }
#'


anthropometrics<-function(gdx,indicator="bodyheight", age="adults", sex=FALSE,bmi_groups=FALSE, level="iso",dir=".",spamfiledirectory = "", final=TRUE,file=NULL,calibrated=TRUE){

  dir <- getDirectory(dir,spamfiledirectory)

  pop<-population(gdx, age = TRUE,sex=TRUE,level="iso")
  underaged<-readGDX(gdx,"underaged15")
  working<-readGDX(gdx,"working15")
  retired<-readGDX(gdx,"retired15")
  adults<-setdiff(readGDX(gdx,"age"),underaged)

  if(indicator=="bmi_shr"){
    if(bmi_groups==FALSE){stop("bmi_groups should be set to true.")}
    if(calibrated==FALSE){
      x<-collapseNames(readGDX(gdx,"p15_bmi_shr_regr"))
    } else if(calibrated==TRUE) {
      x<-collapseNames(readGDX(gdx,"p15_bmi_shr_calibrated"))
    } else {stop("calibrated has to be true or false")}
    x<-x[,,c( "verylow","low","medium","mediumhigh","high","veryhigh")]
  } else if(indicator=="bodyheight"){
    if(calibrated==FALSE){stop("uncalibrated not yet implemented")}
    x<-readGDX(gdx,"p15_bodyheight")
    if(final==TRUE){
      x<-collapseNames(x[,,"final"])
    } else {
      x<-collapseNames(x[,,"preliminary"])
    }
  } else if (indicator=="bodyweight") {
    x = readGDX(gdx=gdx,"p15_bodyweight")
  } else if(indicator=="BMI"){
    x<-readGDX(gdx,"f15_bmi")
  } else if(indicator=="PAL"){
    if(calibrated==FALSE){stop("uncalibrated not yet implemented")}
    x=readGDX(gdx,"p15_physical_activity_level")
  } else {stop("unkown indicator")}

  if(bmi_groups==FALSE) {
    bmi_shr=anthropometrics(gdx,indicator = "bmi_shr",age = TRUE,bmi_groups = TRUE,sex = TRUE)
    pop=pop*bmi_shr
    x<-dimSums(x*pop,dim="bmi_group15")/dimSums(pop,dim="bmi_group15")
    pop<-dimSums(pop,dim="bmi_group15")
  } else {
    if(bmi_groups!=TRUE){stop("bmi_groups has to be TRUE or FALSE")}
    if(!indicator%in%c("bmi_shr","BMI","bodyweight")) { stop("bmi_groups so far only exist for body weight, BMI and bmi_shr")}
  }

  # subselecting and weighting

  if(age=="adults"){
    x=x[,,adults]
    pop<-pop[,,adults]
    age=FALSE
  } else if (age=="underaged"){
    x=x[,,underaged]
    pop<-pop[,,underaged]
    age=FALSE
  } else if (age=="working"){
    x=x[,,working]
    pop<-pop[,,working]
    age=FALSE
  } else if (age=="retired"){
    x=x[,,retired]
    pop<-pop[,,retired]
    age=FALSE
  } else if(!age%in%c(TRUE,FALSE)){
    x=x[,,age]
    pop<-pop[,,age]
    age=FALSE
  }


  if (age==FALSE){
    x<-dimSums(x*pop,dim="age")/dimSums(pop,dim="age")
    pop<-dimSums(pop,dim="age")
  } else if (age!=TRUE){stop("unknown selection for age")}
  if(sex==FALSE){
    x<-dimSums(x*pop,dim="sex")/dimSums(pop,dim="sex")
    pop<-dimSums(pop,dim="sex")
  }

  if(level=="grid"){weight=NULL} else {weight=pop}

  x <- gdxAggregate(gdx,x,to=level,weight=weight,absolute=FALSE, dir = dir)
  out(x,file)
}
