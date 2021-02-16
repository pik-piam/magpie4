#' @title NitrogenBudget
#' @description calculates projections of Nitrogen Budgets for Croplands from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param include_emissions TRUE also divides the N surplus into different emissions
#' @param level aggregation level, reg, glo or regglo, cell or grid
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @param debug debug mode TRUE makes some consistency checks between estimates for different resolutions.
#' @author Benjamin Leon Bodirsky
#' @importFrom magpiesets findset
#' @importFrom madrat toolAggregate
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom gdx readGDX
#' @importFrom luscale superAggregate groupAggregate
#' @importFrom mstools toolFertilizerDistribution
#' @examples
#' 
#'   \dontrun{
#'     x <- NitrogenBudget(gdx)
#'   }
#' 


NitrogenBudget<-function(gdx,include_emissions=FALSE,level="reg",dir=".",spamfiledirectory="",debug=FALSE){

  dir <- getDirectory(dir,spamfiledirectory)
  
  if(level%in%c("cell","reg")){
    
    kcr<-findset("kcr")
    harvest_detail = production(gdx, products="kcr", attributes="nr", level=level)
    harvest = dimSums(harvest_detail,dim=c(3))
    
    # ag <- dimSums(readGDX(gdx,"ov_res_biomass_ag",select=list(type="level"))[,,"nr"][,,kcr],dim=3)
    # bg <- dimSums(readGDX(gdx,"ov_res_biomass_bg",select=list(type="level"))[,,"nr"][,,kcr],dim=3)
    res_detail <- collapseNames(ResidueBiomass(gdx,product_aggr = F,attributes = "nr", level=level))
    res <- dimSums(res_detail,dim=3.2)
    ag <- res[,,"ag"]
    bg <- res[,,"bg"]
    seed_detail <- Seed(gdx,level=level,attributes = "nr")
    seed <- dimSums(Seed(gdx,level=level,attributes = "nr"),dim=3)
    
    ag_recycling <-dimSums(readGDX(gdx,"ov18_res_ag_recycling",select=list(type="level"))[,,"nr"],dim=c(3.1,3.2))
    ag_recycling <- gdxAggregate(gdx = gdx,weight = 'ResidueBiomass',x = ag_recycling, to = level,absolute = TRUE,dir = dir, product_aggr = T,attributes = "nr",plantpart="ag")
    
    ash<- dimSums((readGDX(gdx,"ov18_res_ag_burn",select=list(type="level"))[,,kcr]*(1-readGDX(gdx,"f18_res_combust_eff")[,,kcr]))[,,"nr"],dim=3)
    ash <- gdxAggregate(gdx = gdx,weight = 'ResidueBiomass',x = ash, to = level,absolute = TRUE,dir = dir, product_aggr = T,attributes = "nr",plantpart="ag")
    
    bg_recycling <- bg
    fixation_freeliving <- dimSums(
      croparea(gdx,products = "kcr",product_aggr = FALSE,level=level) * readGDX(gdx, "f50_nr_fix_area")
      ,dim=3)
    
    fixation_crops <- harvest_detail+dimSums(res_detail,dim=3.1)
    #blowup=function(x,format){
    #  warning("temporary fix while magpie expand is bugged.")
    #  format[,,]=0
    #  for(region_x in getRegions(x)){
    #    format[region_x,,] <- x[region_x,,]
    #  }
    #  return(format)
    #}
    fixation_rate = readGDX(gdx,"f50_nr_fix_ndfa")[,getYears(harvest)]
    fixation_crops <- dimSums(fixation_rate*fixation_crops,dim=3)
    
      
    balanceflow<-readGDX(gdx,"f50_nitrogen_balanceflow")[,getYears(harvest),]
    balanceflow <-gdxAggregate(gdx = gdx,weight = 'land',x = balanceflow, to = level,absolute = TRUE,dir = dir, types="crop")
    
    som <-readGDX(gdx,"ov_nr_som_fertilizer",select=list(type="level"), format="first_found")
    som <-gdxAggregate(gdx = gdx,weight = 'land',x = som, to = level,absolute = TRUE,dir = dir, types="crop")
    
    
    manure_confinement = readGDX(gdx,"ov_manure_confinement",select=list(type="level"))[,,"nr"]
    recycling_share = readGDX(gdx,"i55_manure_recycling_share")[,,"nr"]
    manure_recycling = dimSums(manure_confinement * recycling_share,dim=c(3.2,3.3))
    manure_recycling <- gdxAggregate(gdx = gdx,weight = 'production',x = manure_recycling,to = level,absolute = TRUE,dir = dir, products=readGDX(gdx,"kli"), product_aggr=FALSE)
    manure<- dimSums(manure_recycling,dim=3)
    
    croplandgrazing<-dimSums(readGDX(gdx,"ov_manure",select=list(type="level"))[,,"stubble_grazing"][,,"nr"],dim=c(3.2,3.3))
    croplandgrazing <- gdxAggregate(gdx = gdx,weight = 'production',x = croplandgrazing,to = level,absolute = TRUE,dir = dir, products=readGDX(gdx,"kli"), product_aggr=FALSE)
    croplandgrazing <- dimSums(croplandgrazing,dim=3)
    
    dep<-readGDX(gdx,"ov50_nr_deposition")[,,"crop"][,,"level"]
    dep <- gdxAggregate(gdx = gdx,weight = 'land',x = dep,to = level,absolute = TRUE,dir = dir, types="crop")
    
    #dimSums(readGDX(gdx,"ov50_nr_dep_crop",select=list(type="level"))
    
    fertilizer <- collapseNames(readGDX(gdx,"ov_nr_inorg_fert_reg",format="first_found",select=list(type="level"))[,,"crop"])
    
    
    out<-mbind(
      setNames(harvest,"harvest"),
      setNames(ag,"ag"),
      setNames(bg,"bg"),
      #setNames(fertilizer,"fertilizer"),
      setNames(fixation_crops,"fixation_crops"),
      setNames(fixation_freeliving,"fixation_freeliving"),
      setNames(ag_recycling,"ag_recycling"),
      setNames(ash,"ag_ash"),
      setNames(bg_recycling,"bg_recycling"),
      setNames(som,"som"),
      setNames(seed,"seed"),
      setNames(manure,"manure"),
      setNames(croplandgrazing,"manure_stubble_grazing"),
      setNames(dep,"deposition"),
      setNames(balanceflow,"balanceflow")
    )
    
    ### distribution of fertilizer
    
    if(level%in%c("cell")){
      withdrawals = dimSums(mbind(
        out[,,c("harvest","ag","bg")],
        -out[,,c("seed","fixation_crops")]
      ),dim=3)
      organicinputs=dimSums(out[,,c("fixation_freeliving", "ag_recycling", "ag_ash",
                       "bg_recycling","som","seed", "manure","manure_stubble_grazing", 
                       "deposition","balanceflow")],dim=3)
      
      SNUpE = readGDX(gdx,"ov50_nr_eff","ov_nr_eff")[,,"level"]
      
      mapping=readGDX(gdx,"cell")
      max_snupe=0.85
      
      fert=toolFertilizerDistribution(iteration_max=40, max_snupe=0.85, 
                                      mapping=mapping, from="j", to="i", fertilizer=fertilizer, SNUpE=SNUpE, 
                                      withdrawals=withdrawals, organicinputs=organicinputs)
  
    }   else {
      fert=gdxAggregate(x=fertilizer,gdx = gdx,to = level,absolute = T,dir = dir)
    }
    
    out<-mbind(out,setNames(fert,"fertilizer"))
    
    
    ### surplus and emissions
    
    out<-mbind(
      out,
      setNames(
        dimSums(out[,,c("harvest","ag","bg"),invert=TRUE],dim=3)
        - dimSums(out[,,c("harvest","ag","bg")],dim=3) 
        ,"surplus"
      )
    )
    
    if (include_emissions){
      emissions = Emissions(gdx,type = c("n2o_n","nh3_n","no2_n","no3_n"),level = "reg",unit = "element",subcategories = TRUE,lowpass = FALSE,inorg_fert_split = TRUE,cumulative = FALSE)
      types=c("man_crop","resid","rice","inorg_fert_crop")
      emissions = emissions[,,types]
      emissions = dimSums(emissions,dim="emis_source")
      
      if (level=="cell") {
        emissions = gdxAggregate(gdx = gdx,x = emissions,weight = dimSums(out[,,"surplus"]),to = "cell",absolute = TRUE)
      }
      
      out<-mbind(out, emissions) 
    }
    
    
    ### error checks
    if(level=="reg"){
      # withdrawals from gams
      check_out<- readGDX(gdx,"ov50_nr_withdrawals")[,,"level"]
      # withdrawals from postprocessing
      check_out2<-dimSums(out[,,c("harvest","ag","bg")],dim=3) - dimSums(out[,,c("fixation_crops","seed")],dim=3)
      # other form of calculating withdrawals
      check_out3a<-dimSums(
        out[,,c(
          "fertilizer", "fixation_freeliving",
          "ag_recycling","ag_ash", "bg_recycling","som", "manure",               
          "manure_stubble_grazing","deposition","balanceflow")]
      )
      check_out3b<-readGDX(gdx,"ov50_nr_eff","ov_nr_eff")[,,"level"]*check_out3a
      check_out3c<-readGDX(gdx,"ov_nr_eff")[,,"level"] * readGDX(gdx,"ov50_nr_inputs",select=list(type="level"))
      # other form of calculating withdrawals
      check_out4<- 
        (1-readGDX(gdx,"f50_nr_fix_ndfa")[,getYears(harvest),])*(
          readGDX(gdx,"ov_prod_reg")[,,"level"][,,kcr]*collapseNames(readGDX(gdx,"fm_attributes")[,,kcr][,,"nr"])
          +readGDX(gdx,"ov_res_biomass_ag",select=list(type="level"))[,,"nr"][,,kcr]
          +readGDX(gdx,"ov_res_biomass_bg",select=list(type="level"))[,,"nr"][,,kcr]
        )-dimSums((readGDX(gdx,"ov_dem_seed",select=list(type="level"))* readGDX(gdx,"fm_attributes")[,,"nr"])[,,kcr],dim="attributes")
      check_out5<-readGDX(gdx,"ov50_nr_inputs",select=list(type="level"))
      check_out6<-readGDX(gdx,"ov50_nr_surplus_cropland",select=list(type="level"))
      check_out7 = out[,,"surplus"]
      check_out8 = readGDX(gdx,"ov50_nr_surplus_cropland",format="first_found",select=list(type="level"))
      check_out9 = dimSums(out[,,c("n2o_n","nh3_n","no2_n","no3_n")],dim=3)
      
      
      if(sum(abs(dimSums(check_out,dim=3)-check_out2))>0.1){warning("Withdrawals from gams and postprocessing dont match")}
      if(any(dimSums(check_out3c,dim=3)-dimSums(check_out,dim=3) < (-10^-5))){warning("Input or withdrawal calculations in gams have changed and postprocessing should be adapted")}
      if(max(dimSums(check_out3c,dim=3)-dimSums(check_out,dim=3)) > 0.5){warning("Inputs exceed withdrawals by more than 0.5 Tg")}
      if(any(check_out3b - dimSums(check_out,dim=3) < (-10^-5))){warning("Input calculations in gams have changed and postprocessing should be adapted")}
      if(sum(abs(dimSums(check_out3a,dim=3)-dimSums(check_out5,dim=3)))>0.1){warning("Input or withdrawal calculations in gams have changed and postprocessing should be adapted")}
      if(sum(abs(check_out-check_out4))>0.1){warning("Withdrawal calculations in gams have changed and postprocessing should be adapted")}
      if(sum(abs(check_out8-check_out7))>0.1){warning("Surplus in gams and postprocessing dont match")}
      if (include_emissions){
        if(any((check_out7-check_out9)<0)){warning("Emissions exceed surplus. Maybe use rescale realization of 51_nitrogen")}
        if(any(((check_out9+0.5*10^-10)/(check_out7+10^-10))>0.9)){warning("N2 emissions in surplus very low")}
      }
      
      
      ### End of checks
    } else if (level=="cell") {
      reg = NitrogenBudget(gdx=gdx,level="reg")
      diff=superAggregate(data = out,aggr_type = "sum",level = "reg")-reg
      
      if(debug){
        if(any(diff>0.2)) {
          print(where(diff>0)$true)
          warning("cellular and regional aggregates diverge by more than 0.2 Tg N")
        }
      }
    }
    return(out)
  } else if (level=="grid"){
    budget<-NitrogenBudget(gdx,level="cell",include_emissions=include_emissions)
    #out<-production(gdx,level="cell",products = "kli")
    # disaggregate Budget using production as proxy
    budget_grid <-  gdxAggregate(gdx = gdx,x = budget,weight = 'production',to = "grid",
                         absolute = TRUE,dir = dir,
                         attributes = "nr",products = "kcr",product_aggr = TRUE)
    
    if(debug){
      reg = NitrogenBudget(gdx=gdx,level="reg",include_emissions=include_emissions)
      diff=superAggregate(data = budget_grid,aggr_type = "sum",level = "reg")-reg
      if(any(diff>0.1)) {
        print(where(diff>0)$true)
        warning("cellular and regional aggregates diverge by more than 0.1")
      }
    }
    out = budget_grid
    return(out)
  } else if (level=="glo") {
    out<-NitrogenBudget(gdx,level="reg")
    out<-dimSums(out,dim=1)
  } else if (level=="regglo"){
    out<-NitrogenBudget(gdx,level="reg")
    out<-mbind(out, dimSums(out,dim=1))
    return(out)
  }
  
}

