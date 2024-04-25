#' @title NitrogenBudgetNonagland
#' @description calculates projections of Nitrogen Budgets for non-agricutlural land from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level, reg, glo or regglo
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @author Benjamin Leon Bodirsky, Edna J. Molina Bacca
#' @importFrom magpiesets findset
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom gdx readGDX
#' @importFrom mstools toolHoldConstant
#' @examples
#'
#'   \dontrun{
#'     x <- NitrogenBudgetNonagland(gdx)
#'   }
#'


NitrogenBudgetNonagland<-function(gdx,level="reg",dir="."){


  nonagland=c("forestry","primforest", "secdforest", "urban","other")



  if(level!="grid"){
   message("for now the nonag nitrogen budgets have to be calculated from grid-level data. this can be computationaly expensive and requires cell input data.")
  }
  landarea=land(gdx,level="grid",types=nonagland,dir = dir)

  ### deposition
  dep_rate=read.magpie(file.path(dir,"f50_AtmosphericDepositionRates_0.5.mz"))[,,nonagland]
  getCells(dep_rate)=getCells(landarea)
  dep   <- landarea  * collapseNames(dep_rate[,getYears(landarea),])

  ### fixation
  #fix_rate=setYears(calcOutput("NitrogenFixationRateNatural",cellular=TRUE,aggregate=FALSE)[,"y2010",],NULL)
  fix_rate = read.magpie(file.path(dir,"f50_NitrogenFixationRateNatural_0.5.mz"))
  message("fixation rate has only data until 2010. Held constant for the future for now.")
  fix_rate <- toolHoldConstant(fix_rate, years = findset("time"))
  getCells(fix_rate)=getCells(landarea)
  fix   <- landarea  * fix_rate[,getYears(landarea),]


    #dep   <- collapseNames(readGDX(gdx,"ov50_nr_deposition")[,,nonagland][,,"level"])
    #dep   <- gdxAggregate(gdx = gdx,weight = 'land',x = dep,to = level,absolute = TRUE,dir = dir, types=nonagland)
    #fix_rate=readGDX(gdx,"f50_NitrogenFixationRateNatural")[,getYears(harvest),]
    #fix_rate=readGDX(gdx,"")[,,nonagland]
    #fix   <- land(gdx,level="reg",dir = dir)[,,c("past","crop"),invert=TRUE] * fix_rate
    #fix   <- gdxAggregate(gdx = gdx,weight = 'land',x = fix,to = level,absolute = TRUE,dir = dir,types=nonagland)


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

  out=gdxAggregate(gdx,out,to=level, absolute=TRUE,dir=dir)

  return(out)
}
