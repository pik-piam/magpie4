#' @title PlantationEstablishment
#' @description reads carbon stocks in harvested timber out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Area newly established in current time step for future timber production
#' @return Area newly for timber production
#' @author Abhijeet Mishra
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- PlantationEstablishment(gdx)
#'   }

PlantationEstablishment <- function(gdx, file=NULL, level="cell"){
  
  #ac_additional <- readGDX(gdx,"ac_additional") -- AC additional doens't have a time component so we can't sum over it in every step
  
  v32_land <- collapseNames(readGDX(gdx,"ov32_land",select = list(type="level"))[,,"plant"])
  
  if(max(readGDX(gdx,"ov_supply")[,,"level"][,,readGDX(gdx,"kforestry")])>0){
    # This logical statement is only valid for runs with timber demand turned on.
    # When timber demand is on, the mdoel has to meet certain demand with plantations.
    # Additionaly, plantations are added regularly to the timber plantations pool in ac0.
    # In plantation establishmnet, when time step length are more than 5, not just ac0 is newly established but ac5 as well (if the jump is 10 years).
    # This is done outside optimization but the redistribution of newly established ac0 is made into ac0 and ac5 equally. This should refelcet in p32_land
    # This means that for 10 year timestep jumps, ac0 and ac5 are established with ac0 carbon density.
    # We make this adjustment here. This will not impact any run where plantations are not added during the model run.
    
    timestep_length <- readGDX(gdx,"im_years",react="silent")
    if(is.null(timestep_length)) timestep_length <- timePeriods(gdx)
    
    for(i in getYears(timestep_length)){
      if(as.numeric(timestep_length[,i,])>5){
        ## Count how big the jump is
        jump <- as.numeric(timestep_length[,i,])/5
        ## See which age classes were additionally added along with ac0 in this jump
        ac_to_fix <- readGDX(gdx,"ac")[1:jump]
        ## Take the additiona age calsses added and add them to ac0
        v32_land[,i,"ac0"] = v32_land[,i,"ac0"] + dimSums(v32_land[,i,ac_to_fix[-1]],dim=3)
      }
    }
  }
  
  v32_land <- collapseNames(v32_land[,,"ac0"])
  
  ## COnvert to annual values
  v32_land <- v32_land/timePeriods(gdx)
  
  a <- setNames(v32_land,"Forestry")

  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  
  out(a,file)
}