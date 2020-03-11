#' @title NitrogenBudgetPasture
#' @description calculates projections of Nitrogen Budgets for Croplands from a MAgPIE gdx file
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
#' @importFrom mstools toolFertilizerDistribution
#' @examples
#' 
#'   \dontrun{
#'     x <- NitrogenBudgetPasture(gdx)
#'   }
#' 


NitrogenBudgetPasture<-function(gdx,level="reg",spamfiledirectory=""){
  
  if(level!="grid"){
    
  
    harvest    <- production(gdx,level = level,attributes = "nr",products = "pasture")
    fertilizer <- collapseNames(readGDX(gdx,"ov_nr_inorg_fert_reg",format="first_found",select=list(type="level"))[,,"past"])
    
    manure     <- dimSums(readGDX(gdx,"ov_manure",select=list(type="level"))[,,"grazing"][,,"nr"],dim=c(3.2,3.3))
    manure     <- gdxAggregate(gdx = gdx,weight = 'production',x = manure,to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, products=readGDX(gdx,"kli"), product_aggr=FALSE)
    manure     <- dimSums(manure,dim=3.1)
    
    #land  <- land(gdx,level="cell")[,,"past"]
    #dep_rate <- readGDX(gdx, "i50_atmospheric_deposition_rates")
    dep   <- collapseNames(readGDX(gdx,"ov50_nr_deposition")[,,"past"][,,"level"])
    dep   <- gdxAggregate(gdx = gdx,weight = 'land',x = dep,to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, types="past")
    
    fix   <- land(gdx)[,,"past"] * readGDX(gdx,"f50_nr_fixation_rates_pasture")[,getYears(harvest),]
    fix   <- gdxAggregate(gdx = gdx,weight = 'production',x = fix,to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, products="pasture",attributes="nr")
    
    out<-mbind(
      setNames(harvest,"harvest"),
      setNames(manure,"grazing"),
      setNames(fix,"fixation_freeliving"),
      setNames(dep,"deposition")
    )
    
    
    if(level%in%c("cell","grid")){
      withdrawals = harvest
      organicinputs=dimSums(out[,,c("grazing","fixation_freeliving","deposition")],dim=3)
      
      NUE = readGDX(gdx,"ov50_nr_eff_pasture")[,,"level"]
      mapping=readGDX(gdx,"cell")
      max_snupe=0.85
      
      fert=toolFertilizerDistribution(iteration_max=30, max_snupe=0.85, 
                                      mapping=mapping, from="j", to="i", fertilizer=fertilizer, SNUpE=NUE, 
                                      withdrawals=withdrawals, organicinputs=organicinputs)
      
    }   else {
      fert=gdxAggregate(x=fertilizer,gdx = gdx,to = level,absolute = T)
    }
    ###
    out<-mbind(out,setNames(fert,"fertilizer"))
    
    
    out<-mbind(
      out,
      setNames(
        dimSums(out[,,c("harvest"),invert=TRUE],dim=3)
        -dimSums(out[,,c("harvest")],dim=3)
        ,"surplus"
      )
    )
  } else {
    out<-NitrogenBudgetPasture(gdx,level="cell")
    #out<-production(gdx,level="grid",products = "kli")
    out <-  gdxAggregate(gdx = gdx,x = out,weight = 'production',to = "grid",
                         absolute = TRUE,spamfiledirectory = spamfiledirectory,
                         attributes = "nr",products = "pasture",product_aggr = TRUE)
    #out <-  gdxAggregate(gdx = gdx,x = out,weight = 'land',to = "grid",
    #                     absolute = TRUE,spamfiledirectory = spamfiledirectory,
    #                     types="past")
    #land <- land(gdx,level = "grid",types = "past",spamfiledirectory = spamfiledirectory)
    #plotmap2(out[,2010,"fertilizer"]/(land[,2010,]+0.0001))
    reg = NitrogenBudgetPasture(gdx=gdx,level="reg")
    diff=superAggregate(data = out,aggr_type = "sum",level = "reg")-reg
    if(any(abs(diff)>0.1)) {
      print(where(abs(diff)>0.1)$true)
      warning("cellular and regional aggregates diverge by more than 0.1")
    }
    return(out)
  }
  return(out)

}
