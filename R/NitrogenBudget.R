#' @title NitrogenBudget
#' @description calculates projections of Nitrogen Budgets for Croplands from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level aggregation level, reg, glo or regglo, cell or grid
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @author Benjamin Leon Bodirsky
#' @importFrom magpiesets findset
#' @importFrom madrat toolAggregate
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom gdx readGDX
#' @importFrom luscale superAggregate groupAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- NitrogenBudget(gdx)
#'   }
#' 


NitrogenBudget<-function(gdx,level="reg",spamfiledirectory=""){

  kcr<-findset("kcr")
  harvest_detail = production(gdx, products="kcr", level=level)*collapseNames(readGDX(gdx,"fm_attributes")[,,kcr][,,"nr"])
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
  ag_recycling <- gdxAggregate(gdx = gdx,weight = 'ResidueBiomass',x = ag_recycling, to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, product_aggr = T,attributes = "nr",plantpart="ag")
  
  ash<- dimSums((readGDX(gdx,"ov18_res_ag_burn",select=list(type="level"))[,,kcr]*(1-readGDX(gdx,"f18_res_combust_eff")[,,kcr]))[,,"nr"],dim=3)
  ash <- gdxAggregate(gdx = gdx,weight = 'ResidueBiomass',x = ash, to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, product_aggr = T,attributes = "nr",plantpart="ag")
  
  bg_recycling <- bg
  fixation_freeliving <- dimSums(
    croparea(gdx,products = "kcr",product_aggr = FALSE,level=level) * readGDX(gdx, "f50_nr_fix_area")
    ,dim=3)
  fixation_crops <- dimSums(
    readGDX(gdx,"f50_nr_fix_ndfa")[,getYears(harvest)]*(
      harvest_detail+res_detail-seed
    ),dim=3)
  
  
  balanceflow<-readGDX(gdx,"f50_nitrogen_balanceflow")[,getYears(harvest),]
  balanceflow <-gdxAggregate(gdx = gdx,weight = 'land',x = balanceflow, to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, types="crop")
  
  som <-readGDX(gdx,"ov_nr_som",select=list(type="level"), format="first_found")
  som <-gdxAggregate(gdx = gdx,weight = 'land',x = som, to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, types="crop")
  
  
  manure_confinement = readGDX(gdx,"ov_manure_confinement",select=list(type="level"))[,,"nr"]
  recycling_share = readGDX(gdx,"i55_manure_recycling_share")[,,"nr"]
  manure_recycling = dimSums(manure_confinement * recycling_share,dim=c(3.2,3.3))
  manure_recycling <- gdxAggregate(gdx = gdx,weight = 'production',x = manure_recycling,to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, products=readGDX(gdx,"kli"), product_aggr=FALSE)
  manure<- dimSums(manure_recycling,dim=3)
  
  croplandgrazing<-dimSums(readGDX(gdx,"ov_manure",select=list(type="level"))[,,"stubble_grazing"][,,"nr"],dim=c(3.2,3.3))
  croplandgrazing <- gdxAggregate(gdx = gdx,weight = 'production',x = croplandgrazing,to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, products=readGDX(gdx,"kli"), product_aggr=FALSE)
  croplandgrazing <- dimSums(croplandgrazing,dim=3)
  
  dep<-readGDX(gdx,"ov50_nr_deposition")[,,"crop"][,,"level"]
  dep <- gdxAggregate(gdx = gdx,weight = 'land',x = dep,to = level,absolute = TRUE,spamfiledirectory = spamfiledirectory, types="crop")
  
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
  
  if(level%in%c("cell","grid")){
    withdrawals = dimSums(mbind(
      out[,,c("harvest","ag","bg")],
      -out[,,c("seed","fixation_crops")]
    ),dim=3)
    organicinputs=dimSums(out[,,c("fixation_freeliving", "ag_recycling", "ag_ash",
                     "bg_recycling","som","seed", "manure","manure_stubble_grazing", 
                     "deposition","balanceflow")],dim=3)
    
    NUE = readGDX(gdx,"ov50_nr_eff")[,,"level"]
    mapping=readGDX(gdx,"cell")
    max_snupe=0.85
    
    fert<-fertilizer
    iteration_max=20
    for (iteration in 1:iteration_max){
      cat(paste0(" iteration: ",iteration)," ")
      #cat(paste0(" NUE ",round(NUE["DEU",2010,],2))," ")
      requiredinputs=withdrawals/NUE
      requiredinputs[is.nan(requiredinputs)]<-0
      requiredfertilizer = requiredfertilizer_nonnegative= requiredinputs - organicinputs
      requiredfertilizer_nonnegative[requiredfertilizer_nonnegative<0]=0
      # negative required fertilizers indicate that organic fertilizers are sufficent to satisfy the needs. 
      requiredfertilizer_nonnegative_country = toolAggregate(requiredfertilizer_nonnegative,rel=mapping,from="j",to="i",partrel=T)
      surplus_fertilizer=requiredfertilizer_nonnegative_country-fert[getRegions(requiredfertilizer_nonnegative),,]
      cat(paste0("  surplus_fertilizer in 2010:",sum(abs(surplus_fertilizer)[,2010]),";"))
      if(sum(abs(surplus_fertilizer),na.rm=T)>1){  # 1 is an arbitrary threshold
        NUE = (
          groupAggregate(withdrawals,dim = 1,query = mapping,from = "j",to = "i")
          /(groupAggregate(organicinputs+requiredfertilizer,dim = 1,query = mapping,from = "j",to = "i") 
            - surplus_fertilizer
          )
        )
        NUE[is.na(NUE)]<-max_snupe
      } else {
        break
      }
    }
    if(sum(abs(surplus_fertilizer),na.rm=T)>1){cat(1,"fertilizer distribution procedure found no equilibrium")}
    #snupe_cell=withdrawals/(organicinputs+requiredfertilizer_nonnegative)
    fert=requiredfertilizer_nonnegative
  } else {
    fert=gdxAggregate(x=fertilizer,gdx = gdx,to = level,absolute = T)
  }
  
  ###
  out<-mbind(out,setNames(fert,"fertilizer"))
  
  out<-mbind(
    out,
    setNames(
      dimSums(out[,,c("harvest","ag","bg"),invert=TRUE],dim=3)
      -dimSums(out[,,c("harvest","ag","bg")],dim=3)
      ,"surplus"
    )
  )
  
  ### error checks
  if(level=="reg"){
    check_out<- readGDX(gdx,"ov50_nr_withdrawals")[,,"level"]
    check_out2<-dimSums(out[,,c("harvest","ag","bg")],dim=3) - dimSums(out[,,c("fixation_crops","seed")],dim=3)
    check_out3<-readGDX(gdx,"ov50_nr_eff")[,,"level"]*dimSums(
      out[,,c(
        "fertilizer", "fixation_freeliving",
        "ag_recycling","ag_ash", "bg_recycling","som", "manure",               
        "manure_stubble_grazing","deposition","balanceflow")]
    )
    check_out4<- 
      (1-readGDX(gdx,"f50_nr_fix_ndfa")[,getYears(harvest),])*(
        readGDX(gdx,"ov_prod_reg")[,,"level"][,,kcr]*collapseNames(readGDX(gdx,"fm_attributes")[,,kcr][,,"nr"])
        +readGDX(gdx,"ov_res_biomass_ag",select=list(type="level"))[,,"nr"][,,kcr]
        +readGDX(gdx,"ov_res_biomass_bg",select=list(type="level"))[,,"nr"][,,kcr]
      )-dimSums((readGDX(gdx,"ov_dem_seed",select=list(type="level"))* readGDX(gdx,"fm_attributes")[,,"nr"])[,,kcr],dim="attributes")
    
    if(sum(abs(dimSums(check_out,dim=3)-check_out2))>0.1){warning("There are inconsistencies in the reporting function NitrogenBudget")}
    if(sum(abs(dimSums(check_out,dim=3)-check_out3))>0.1){warning("There are inconsistencies in the reporting function NitrogenBudget")}
    if(sum(abs(check_out-check_out4))>0.1){warning("There are inconsistencies in the reporting function NitrogenBudget")}
    ### End of checks
  }
  
  
  return(out)

}