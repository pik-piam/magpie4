#' @title NitrogenBudget
#' @description calculates projections of Nitrogen Budgets for Croplands from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level aggregation level, reg, glo or regglo
#' @author Benjamin Leon Bodirsky
#' @importFrom magpiesets findset
#' @importFrom magclass dimSums collapseNames mbind
#' @importFrom gdx readGDX
#' @importFrom luscale superAggregate groupAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- NitrogenBudget(gdx)
#'   }
#' 


NitrogenBudget<-function(gdx,level="reg"){
  kcr<-findset("kcr")
  harvest<-dimSums(production(gdx)[,,kcr]*collapseNames(readGDX(gdx,"fm_attributes")[,,kcr][,,"nr"]),dim=c(3))
  ag <- dimSums(readGDX(gdx,"ov_res_biomass_ag",select=list(type="level"))[,,"nr"][,,kcr],dim=3)
  bg <- dimSums(readGDX(gdx,"ov_res_biomass_bg",select=list(type="level"))[,,"nr"][,,kcr],dim=3)
  fertilizer <- collapseNames(readGDX(gdx,"ov_nr_inorg_fert_reg",format="first_found",select=list(type="level"))[,,"crop"])
  ag_recycling <-dimSums(readGDX(gdx,"ov18_res_ag_recycling",select=list(type="level"))[,,"nr"],dim=c(3.1,3.2))
  ash<- dimSums((readGDX(gdx,"ov18_res_ag_burn",select=list(type="level"))[,,kcr]*(1-readGDX(gdx,"f18_res_combust_eff")[,,kcr]))[,,"nr"],dim=3)
  bg_recycling <- bg
  fixation_freeliving <- dimSums(
    croparea(gdx,products = "kcr",product_aggr = FALSE) * readGDX(gdx, "f50_nr_fix_area")
    ,dim=3)
  fixation_crops <- dimSums(
    readGDX(gdx,"f50_nr_fix_ndfa")[,getYears(harvest)]*(
      production(gdx)[,,kcr]*collapseNames(readGDX(gdx,"fm_attributes")[,,kcr][,,"nr"])
      +readGDX(gdx,"ov_res_biomass_ag",select=list(type="level"))[,,"nr"][,,kcr]
      +readGDX(gdx,"ov_res_biomass_bg",select=list(type="level"))[,,"nr"][,,kcr]
    ),dim=3)
  balanceflow<-readGDX(gdx,"f50_nitrogen_balanceflow")[,getYears(harvest),]
  som <-groupAggregate(readGDX(gdx,"ov_nr_som_fertilizer", "ov_nr_som",select=list(type="level"), format="first_found"),vectorfunction = "sum",dim = 1,to = "reg",query = "cluster_reg")
  seed <- dimSums((readGDX(gdx,"ov_dem_seed",select=list(type="level"))* readGDX(gdx,"fm_attributes"))[,,kcr][,,"nr"],dim=c(3.1,3.2))
  manure<-readGDX(gdx,"ov_manure_recycling",select=list(type="level"))[,,"nr"]
  croplandgrazing<-dimSums(readGDX(gdx,"ov_manure",select=list(type="level"))[,,"stubble_grazing"][,,"nr"],dim=c(3.1,3.2,3.3))
  dep<-readGDX(gdx,"ov50_nr_deposition")[,,"crop"][,,"level"]
  #dimSums(readGDX(gdx,"ov50_nr_dep_crop",select=list(type="level"))
  
  out<-mbind(
    setNames(harvest,"harvest"),
    setNames(ag,"ag"),
    setNames(bg,"bg"),
    setNames(fertilizer,"fertilizer"),
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
  out<-mbind(
    out,
    setNames(
      dimSums(out[,,c("harvest","ag","bg"),invert=TRUE],dim=3)
      -dimSums(out[,,c("harvest","ag","bg")],dim=3)
      ,"surplus"
    )
  )
  
  ### error checks
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
  
  
  if(level!="reg") out <- superAggregate(out,aggr_type="sum",level=level)
  return(out)

}