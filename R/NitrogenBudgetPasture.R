#' @title NitrogenBudgetPasture
#' @description calculates projections of Nitrogen Budgets for Croplands from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param include_emissions TRUE also divides the N surplus into different emissions
#' @param level aggregation level, reg, glo or regglo
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @author Benjamin Leon Bodirsky
#' @importFrom magpiesets findset
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom gdx readGDX
#' @importFrom luscale superAggregate groupAggregate
#' @importFrom mstools toolFertilizerDistribution
#' @examples
#' 
#'   \dontrun{
#'     x <- NitrogenBudgetPasture(gdx)
#'   }
#' 


NitrogenBudgetPasture <- function(gdx,include_emissions=FALSE,level="reg",dir=".",spamfiledirectory=""){
  dir <- getDirectory(dir,spamfiledirectory)
  #if(level!="grid"){
    
  harvest    <- production(gdx,level = level,attributes = "nr",products = "pasture",dir = dir)
  fertilizer <- collapseNames(readGDX(gdx,"ov_nr_inorg_fert_reg",format="first_found",select=list(type="level"))[,,"past"])
  
  manure     <- dimSums(readGDX(gdx,"ov_manure",select=list(type="level"))[,,"grazing"][,,"nr"],dim=c(3.2,3.3))
  manure     <- gdxAggregate(gdx = gdx,weight = 'ManureExcretion',x = manure,to = level,absolute = TRUE,dir = dir, products=readGDX(gdx,"kli"), awms="grazing", agg="awms")
  manure     <- dimSums(manure,dim=3)
  
  #land  <- land(gdx,level="cell")[,,"past"]
  #dep_rate <- readGDX(gdx, "i50_atmospheric_deposition_rates")
  dep   <- collapseNames(readGDX(gdx,"ov50_nr_deposition")[,,"past"][,,"level"])
  dep   <- gdxAggregate(gdx = gdx,weight = 'land',x = dep,to = level,absolute = TRUE,dir = dir, types="past")
  
  fix   <- land(gdx)[,,"past"] * readGDX(gdx,"f50_nr_fixation_rates_pasture")[,getYears(harvest),]
  fix   <- gdxAggregate(gdx = gdx,weight = 'production',x = fix,to = level,absolute = TRUE,dir = dir, products="pasture",attributes="nr")
  
  out<-mbind(
    setNames(harvest,"harvest"),
    setNames(manure,"grazing"),
    setNames(fix,"fixation_freeliving"),
    setNames(dep,"deposition")
  )
  
  
  if(level%in%c("cell","grid")){
    withdrawals = out[,,"harvest"]
    organicinputs=dimSums(out[,,c("grazing","fixation_freeliving","deposition")],dim=3)
    
    NUE = readGDX(gdx,"ov50_nr_eff_pasture","ov_nr_eff_pasture")[,,"level"]
    if(level=="cell"){
      mapping=readGDX(gdx,"cell")
    } else if (level=="grid"){
      mapping=retrieve_spamfile(gdx=gdx,dir=dir)
      mapping=mapping[,c(1,3)]
      names(mapping)=c("i","j")
    }
    
    max_snupe=0.85
    
    fert=toolFertilizerDistribution(iteration_max=50, max_snupe=max_snupe, 
                                    mapping=mapping, from="j", to="i", fertilizer=fertilizer, SNUpE=NUE, 
                                    withdrawals=withdrawals, organicinputs=organicinputs)
    
  }   else {
    fert=gdxAggregate(x=fertilizer,gdx = gdx,to = level,absolute = T)
  }
  
  out<-mbind(out,setNames(fert,"fertilizer"))
  
  ### surplus and emissions
  out<-mbind(
    out,
    setNames(
      dimSums(out[,,c("harvest"),invert=TRUE],dim=3)
      -dimSums(out[,,c("harvest")],dim=3)
      ,"surplus"
    )
  )
  if(any(out<0)){
    warning("due to non-iteration of fertilizer distribution, residual fertilizer deficit is moved to balanceflow.")
    balanceflow=out[,,"surplus"]
    balanceflow[balanceflow>0]=0
    out[,,"surplus"] = out[,,"surplus"] - balanceflow
    out = mbind(out,setNames(balanceflow,"balanceflow"))
  }
  
  if (include_emissions){
    emissions = Emissions(gdx,type = c("n2o_n","nh3_n","no2_n","no3_n"),level = "reg",unit = "element",subcategories = TRUE,lowpass = FALSE,inorg_fert_split = TRUE,cumulative = FALSE)
    types=c("man_past")
    emissions = emissions[,,types]
    emissions = dimSums(emissions,dim="emis_source")
    
    emissions = gdxAggregate(gdx = gdx,x = emissions,weight = dimSums(out[,,"surplus"]),to = level,absolute = TRUE)
    
    out<-mbind(out, emissions) 
  }
  
  
  ### error checks
  if(level=="reg"){
    
    out_surplus = out[,,"surplus"]
    ov50_nr_surplus_pasture = readGDX(gdx,"ov50_nr_surplus_pasture",format="first_found",select=list(type="level"))
    
    if(sum(abs(out_surplus-ov50_nr_surplus_pasture))>0.1){warning("Surplus in gams and postprocessing dont match")}
    if (include_emissions){
      out_emis = dimSums(out[,,c("n2o_n","nh3_n","no2_n","no3_n")],dim=3)
      if(any((ov50_nr_surplus_pasture-out_emis)<0)){warning("Emissions exceed surplus. Maybe use rescale realization of 51_nitrogen")}
      if(any(((out_emis+0.5*10^-10)/(out_surplus+10^-10))>0.9)){warning("N2 emissions in surplus very low")}
    }
  }  
    
  
  #} else {
  #  out<-NitrogenBudgetPasture(gdx,level="cell")
  #  #out<-production(gdx,level="grid",products = "kli")
  #  out <-  gdxAggregate(gdx = gdx,x = out,weight = 'production',to = "grid",
  #                       absolute = TRUE,dir = dir,
  #                       attributes = "nr",products = "pasture",product_aggr = TRUE)
  #  #out <-  gdxAggregate(gdx = gdx,x = out,weight = 'land',to = "grid",
  #  #                     absolute = TRUE,dir = dir,
  #  #                     types="past")
  #  #land <- land(gdx,level = "grid",types = "past",dir = dir)
  #  #plotmap2(out[,2010,"fertilizer"]/(land[,2010,]+0.0001))
  #  reg = NitrogenBudgetPasture(gdx=gdx,level="reg")
  #  diff=superAggregate(data = out,aggr_type = "sum",level = "reg")-reg
  #  if(any(abs(diff)>0.1)) {
  #    print(where(abs(diff)>0.1)$true)
  #    warning("cellular and regional aggregates diverge by more than 0.1")
  #  }
  #}
  return(out)

}
