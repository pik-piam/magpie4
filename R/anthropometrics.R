#' @title anthropometrics
#' @description Calculates anthropometic indicators from the food demand model
#' @param gdx GDX file
#' @param indicator bodyheight, bodyweight, bodyweight_healthy, BMI(Body Mass Index) or PAL (physical activity level)
#' @param level Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)
#' @param age_groups if TRUE, demand is scaled down to age-groups and sex using food requirements
#' @param sex if FALSE, female and male are aggregated, if sex, results are divided into males and females
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @param final final results or preliminary results (the latter are the ones magpie uses for optimization before last iteration with demand model)
#' @param file a file name the output should be written to using write.magpie
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


anthropometrics<-function(gdx,indicator="bodyheight", age_groups="adults", sex=FALSE, level="iso",spamfiledirectory = "", final=TRUE,file=NULL){
  pop<-population(gdx, age_groups = TRUE,sex=TRUE,level="iso")
  underaged<-readGDX(gdx,"age_groups_underaged15")
  adults<-setdiff(readGDX(gdx,"age_group"),underaged)
  if(indicator=="bodyheight"){
    x<-readGDX(gdx,"p15_bodyheight")
    if(final==TRUE){
      x<-collapseNames(x[,,"final"])
    } else {
      x<-collapseNames(x[,,"preliminary"])
    }
  } else if (indicator=="bodyweight_healthy") {
    x<-readGDX(gdx,"p15_bodyweight_healthy")
  } else if (indicator=="bodyweight") {
    intake<-Kcal(gdx=gdx,intake=TRUE,level="iso",age_groups=TRUE,product_aggr = T)
    schofield <- readGDX(gdx=gdx,"f15_schofield_parameters")
    PAL = readGDX(gdx=gdx,"p15_physical_activity_level")
    x=collapseNames((intake/PAL-schofield[,,"intercept"])/schofield[,,"slope"])
  } else if(indicator=="BMI"){
    bodyheight=anthropometrics(gdx = gdx,indicator = "bodyheight",age_groups = TRUE,sex = TRUE,level = "iso",final = TRUE)
    bodyweight=anthropometrics(gdx = gdx,indicator = "bodyweight",age_groups = TRUE,sex = TRUE,level = "iso")
    #bodyweight=anthropometrics(gdx = gdx,indicator = "bodyweight_healthy",age_groups = TRUE,sex = TRUE,level = "iso")
    x=(bodyweight/(bodyheight/100)^2)
  } else if(indicator=="PAL"){
    x=readGDX(gdx,"p15_physical_activity_level")
  }
  
  
  if(age_groups=="adults"){
    x=x[,,adults]
    pop<-pop[,,adults]
    age_groups=FALSE
  } else if (age_groups=="underaged"){
    x=x[,,underaged]
    pop<-pop[,,underaged]
    age_groups=FALSE
  }
  if (age_groups==FALSE){
    x<-dimSums(x*pop,dim="age_group")/dimSums(pop,dim="age_group")
    pop<-dimSums(pop,dim="age_group")
  } else if (age_groups!=TRUE){stop("unknown selection for age_groups")}
  if(sex==FALSE){
    x<-dimSums(x*pop,dim="sex")/dimSums(pop,dim="sex")
    pop<-dimSums(pop,dim="sex")
  }
  if(level=="grid"){weight=NULL} else {weight=pop}
  x=gdxAggregate(gdx,x,to=level,weight=weight,absolute=FALSE,spamfiledirectory = spamfiledirectory)
  out(x,file)
}