#' @title population
#' @description reads population out of a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param age_groups if TRUE, population is split up by age groups
#' @param sex if TRUE, population is split up by sex
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

population <- function(gdx, file=NULL, level="reg",age_groups=FALSE,sex=FALSE,spamfiledirectory="") {
  
  pop <- readGDX(gdx, "im_demography", format="first_found", react="warning")
  
  #check
  pop2 <- readGDX(gdx, "im_pop_iso", format="first_found", react="warning")
  # add one person to each country + age group to avoid division by zeros
  pop=pop+0.000001
  
  if(sum(abs(dimSums(pop,dim=3)-pop2))>1){warning(paste0("datasets for demogragphy and population diverge by: ",sum(abs(dimSums(pop,dim=3)-pop2))/length(getYears(pop))," Mio people in average per timestep"))}
  
  if(age_groups==FALSE){
    pop<-dimSums(pop,dim="age_group")
  }
  if (sex==FALSE){
    pop<-dimSums(pop,dim="sex")
  }
  
  pop=gdxAggregate(gdx,pop,to=level,absolute=TRUE,spamfiledirectory = spamfiledirectory,weight = 'land',subcategories="urban")

  out(pop,file)
}