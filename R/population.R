#' @title population
#' @description reads population out of a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param age if TRUE, population is split up by age groups
#' @param sex if TRUE, population is split up by sex
#' @param bmi_groups if TRUE, the population will be split up in body-mass-index groups.
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @return population as MAgPIE object (million people)
#' @author Florian Humpenoeder, Benjamin Bodirsky
#' @importFrom magclass colSums
#' @seealso \code{\link{reportPopulation}}
#' @examples
#' 
#'   \dontrun{
#'     x <- population(gdx)
#'   }
#' 

population <- function(gdx, file=NULL, level="reg",age=FALSE,sex=FALSE,bmi_groups=FALSE,spamfiledirectory="") {
  
  pop <- readGDX(gdx, "im_demography", format="first_found", react="warning")
  pop=pop+0.000001
  
  #check
  check=FALSE
  if(check){
    pop2 <- readGDX(gdx, "im_pop_iso", format="first_found", react="warning")
    # add one person to each country + age group to avoid division by zeros
    if(sum(abs(dimSums(pop,dim=3)-pop2))>10){warning(paste0("datasets for demogragphy and population diverge by: ",round(sum(abs(dimSums(pop,dim=3)-pop2))/length(getYears(pop)))," Mio people in average per timestep"))}
  }

  #subset years
  pop <- pop[,readGDX(gdx,"t"),]
  
  underaged<-readGDX(gdx,"underaged15")
  working<-readGDX(gdx,"working15")
  retired<-readGDX(gdx,"retired15")
  adults<-setdiff(readGDX(gdx,"age"),underaged)
  if(age==FALSE){
    pop<-dimSums(pop,dim="age")
  } else if(age=="adults"){
    pop<-pop[,,adults]
    pop<-dimSums(pop,dim="age")
  } else if (age=="underaged"){
    pop<-pop[,,underaged]
    pop<-dimSums(pop,dim="age")
  } else if (age=="working"){
    pop<-pop[,,working]
    pop<-dimSums(pop,dim="age")
  } else if (age=="retired"){
    pop<-pop[,,retired]
    pop<-dimSums(pop,dim="age")
  } else if (age!=TRUE) {
    pop<-pop[,,age]
    pop<-dimSums(pop,dim="age")
  }
  
  if (sex==FALSE){
    pop<-dimSums(pop,dim="sex")
  } else if (sex!=TRUE) {
    pop<-pop[,,sex]
    pop<-dimSums(pop,dim="sex")
  }
  if(bmi_groups==TRUE){
    bmi_shr=anthropometrics(gdx = gdx,indicator = "bmi_shr",sex=sex,age=age,bmi_groups=TRUE,level="iso")
    pop=pop*bmi_shr
  } else if (bmi_groups!=FALSE) {
    bmi_shr=anthropometrics(gdx = gdx,indicator = "bmi_shr",sex=sex,age=age,bmi_groups=TRUE,level="iso")
    pop=pop*bmi_shr
    pop<-pop[,,bmi_groups]
    pop<-dimSums(pop,dim="bmi_group15")
  }
  
  pop=gdxAggregate(gdx,pop,to=level,absolute=TRUE,spamfiledirectory = spamfiledirectory,weight = 'land',subcategories="urban")

  out(pop,file)
}