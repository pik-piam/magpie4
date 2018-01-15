#' @title NitrogenBudgetPasture
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
#'     x <- NitrogenBudgetPasture(gdx)
#'   }
#' 


NitrogenBudgetPasture<-function(gdx,level="reg"){
  harvest<-dimSums(production(gdx)[,,"pasture"]*collapseNames(readGDX(gdx,"fm_attributes")[,,"pasture"][,,"nr"]),dim=c(3))
  fertilizer <- collapseNames(readGDX(gdx,"ov_nr_inorg_fert_reg",format="first_found",select=list(type="level"))[,,"past"])
  manure<-dimSums(readGDX(gdx,"ov_manure",select=list(type="level"))[,,"grazing"][,,"nr"],dim=3)
  dep<-readGDX(gdx,"ov50_nr_deposition")[,,"past"][,,"level"]
  fix<- croparea(gdx) * readGDX(gdx,"f50_nr_fixation_rates_pasture")[,getYears(harvest),]
  
  out<-mbind(
    setNames(harvest,"harvest"),
    setNames(fertilizer,"fertilizer"),
    setNames(manure,"grazing"),
    setNames(fix,"fixation_freeliving"),
    setNames(dep,"deposition")
  )
  
  out<-mbind(
    out,
    setNames(
      dimSums(out[,,c("harvest"),invert=TRUE],dim=3)
      -dimSums(out[,,c("harvest")],dim=3)
      ,"surplus"
    )
  )
  
  if(level!="reg") out <- superAggregate(out,aggr_type="sum",level=level)
  return(out)

}