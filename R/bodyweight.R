#' @title bodyweight
#' @description Calculates the prevalence of underweight, normalweight, overweight (excluding obesity) and obesity. For more detailed body mass classifications see functions population or anthropometrics.
#' @param gdx GDX file
#' @param level Level of regional aggregation; "iso" ISO country codes, "reg" (regional), "glo" (global)
#' @param age if TRUE, demand is scaled down to age-groups and sex using food requirements
#' @param sex if FALSE, female and male are aggregated, if sex, results are divided into males and females
#' @param share if TRUE, data is provided by BMI group
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @details Demand definitions are equivalent to FAO Food supply categories
#' @return MAgPIE object with mio people or share of people in each weight category
#' @author Benjamin Leon Bodirsky
#' @importFrom gdx readGDX
#' @importFrom magpiesets findset
#' @importFrom magclass dimSums
#' @export
#' @examples
#' 
#'   \dontrun{
#'     x <- bodyweight(gdx)
#'   }
#' 

bodyweight<-function(gdx,level="reg",age=FALSE,sex=FALSE, share=FALSE,spamfiledirectory=""){
  
  total  <- population(gdx,level="iso",bmi_groups = T,sex=T,age=TRUE)
  all <- total[,,c("verylow","low","medium","mediumhigh")]*0
  getNames(all,dim = 3)=c("underweight","normalweight","overweight","obese")
  
  agg=FALSE
  
  underaged<-readGDX(gdx,"underaged15")
  working<-readGDX(gdx,"working15")
  retired<-readGDX(gdx,"retired15")
  adults<-setdiff(readGDX(gdx,"age"),underaged)
  
  all[,,"underweight"]<-total[,,"verylow"]
  all[,,"underweight"]<-total[,,"verylow"]
  all[,,"overweight"][,,underaged]<-total[,,underaged][,,c("high")]
  all[,,"overweight"][,,adults]<-total[,,adults][,,c("mediumhigh")]
  all[,,"obese"][,,underaged]<-total[,,underaged][,,c("veryhigh")]
  all[,,"obese"][,,adults]<-dimSums(total[,,adults][,,c("high","veryhigh")],dim="bmi_group15")
  all[,,"normalweight"]<-dimSums(total,dim="bmi_group15")-dimSums(all,dim="bmi_group15")
  
  if(sex==FALSE){
    all<-dimSums(all,dim="sex")
  } else if (sex !=TRUE){
    all<-all[,,sex]
  }
  
  if(age==FALSE){
    agg=T
  } else if (age !=TRUE){
    if(age=="underaged"){
      age=underaged
      agg=T}
    if(age=="working"){
      age=working
      agg=T}
    if(age=="retired"){
      age=retired
      agg=T}
    if(age=="adults"){
      age=adults
      agg=T}
    all<-all[,,age]
  }
  
  if(agg==TRUE){
    all<-dimSums(all,dim="age")
  }
  
  all=gdxAggregate(gdx,all,to=level,absolute=TRUE,spamfiledirectory = spamfiledirectory,weight = 'population')
  
  
  if (share==FALSE){
    all = all
  } else {
    all = all / dimSums(all, dim="bmi_group15")
  }
  return(all)
}
