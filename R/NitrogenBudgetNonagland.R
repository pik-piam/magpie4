#' @title NitrogenBudgetNonagland
#' @description calculates projections of Nitrogen Budgets for non-agricutlural land from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level aggregation level, reg, glo or regglo
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @author Benjamin Leon Bodirsky
#' @importFrom magpiesets findset
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom gdx readGDX
#' @importFrom luscale superAggregate groupAggregate
#' @importFrom moinput toolHoldConstantBeyondEnd
#' @examples
#' 
#'   \dontrun{
#'     x <- NitrogenBudgetNonagland(gdx)
#'   }
#' 


NitrogenBudgetNonagland<-function(gdx,level="reg",spamfiledirectory=""){
  
  nonagland=c("forestry","primforest", "secdforest", "urban","other")
  
  if(level=="grid"){
    landarea=land(gdx,level="grid",types=nonagland)
    
    ### deposition
    dep_rate=read.magpie(path(spamfiledirectory,"f50_AtmosphericDepositionRates_0.5.mz"))[,,nonagland]
    getCells(dep_rate)=getCells(landarea)
    dep   <- landarea  * collapseNames(dep_rate[,getYears(landarea),])
    
    ### fixation
    #fix_rate=setYears(calcOutput("NitrogenFixationRateNatural",cellular=TRUE,aggregate=FALSE)[,"y2010",],NULL)
    fix_rate = read.magpie(path(spamfiledirectory,"f50_NitrogenFixationRateNatural_0.5.mz"))
    warning("fixation rate has only data until 2010. Held constant for the future for now.")
    fix_rate <- toolHoldConstantBeyondEnd(fix_rate)
    getCells(fix_rate)=getCells(landarea)
    fix   <- landarea  * fix_rate[,getYears(landarea),]
  } else {
    
    dep   <- collapseNames(readGDX(gdx,"ov50_nr_deposition")[,,nonagland][,,"level"])
    dep   <- gdxAggregate(gdx = gdx,weight = 'land',x = dep,to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, types=nonagland)
    
    #fix_rate=readGDX(gdx,"f50_NitrogenFixationRateNatural")[,getYears(harvest),]
    warning("deposition should be calculated on cell level, but requires clustered inputdata")
    fix_rate=readGDX(gdx,"ic50_atmospheric_deposition_rates")[,,nonagland]
    fix   <- land(gdx,level="reg")[,,c("past","crop"),invert=TRUE] * fix_rate
    fix   <- gdxAggregate(gdx = gdx,weight = 'land',x = fix,to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory,types=nonagland)
  }

  out<-mbind(
    add_dimension(fix,dim = 3.1,add = "budget",nm = "fixation_freeliving"),
    add_dimension(dep,dim = 3.1,add = "budget",nm = "deposition")
  )
    
  out<-mbind(
    out,
    add_dimension(
      dimSums(out,dim=3.1),dim = 3.1,add = "budget",
      nm="surplus"
    )
  )
  
  return(out)

}
