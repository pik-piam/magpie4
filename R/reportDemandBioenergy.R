#' @title reportDemandBioenergy
#' @description reports Bioenergy Demand in EJ/yr
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return Bioenergy demand as MAgPIE object (EJ/yr)
#' @author Florian Humpenoeder, Kristine Karstens
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- reportDemandBioenergy()
#'   }
#' 

reportDemandBioenergy <- function(gdx, detail=FALSE){
  
  # Subdivied bioenergy demand product specific into traditional, 1st generation and 2nd generation
  # This information is just available as in level 'reg' and will be aggregated to 'regglo' 
  
  if(is.null(BE_1st_tra <- readGDX(gdx,"i60_1stgen_bioenergy_dem", react="silent"))){
    out <-  demandBioenergy(gdx,level="regglo")
    y <- dimSums(out, dim = 3)
    getNames(out) <- paste0("Demand|Bioenergy|++|",getNames(out)," (EJ/yr)")
    getNames(y) <- "Demand|Bioenergy (EJ/yr)"
    out <- mbind(out, y)
    
  } else {
  
    gen_tra <- findset("kres")
    gen_1st <- c("oils", "ethanol")
    gen_2nd <- findset(c("bioenergycrops","kres"))
    
    BE_demand   <- collapseNames(demand(gdx,level="regglo",attributes = "ge")[,,"bioenergy"])/1000
    
    BEres_2nd   <- superAggregate(readGDX(gdx, "ov60_2ndgen_bioenergy_dem_residues",  select=list(type="level")), aggr_type="sum", level="regglo")/1000
    BEcrops_2nd <- superAggregate(readGDX(gdx, "ov60_2ndgen_bioenergy_dem_dedicated", select=list(type="level")), aggr_type="sum", level="regglo")/1000
    BE_1st_tra  <- superAggregate(BE_1st_tra, aggr_type="sum", level="regglo")/1000
    
    BE_overflow <- BE_demand - BEcrops_2nd - BEres_2nd - BE_1st_tra
    BE_1st      <- add_columns(BE_1st_tra[,,gen_1st], addnm = setdiff(getNames(BE_1st_tra),gen_1st))      
    BE_1st[is.na(BE_1st)] <- 0
    BE_tra      <- add_columns(BE_1st_tra[,,gen_tra], addnm = setdiff(getNames(BE_1st_tra),gen_tra))      
    BE_tra[is.na(BE_tra)] <- 0
    
    out <- mbind(reporthelper(x= BEres_2nd + BEcrops_2nd, level_zero_name = "Demand|Bioenergy|2nd generation",      detail = detail, dim=3.1),
                 reporthelper(x= BE_1st,                  level_zero_name = "Demand|Bioenergy|1st generation",      detail = detail, dim=3.1),
                 reporthelper(x= BE_tra,                  level_zero_name = "Demand|Bioenergy|Traditional Burning", detail = detail, dim=3.1),
                 reporthelper(x= BE_overflow,             level_zero_name = "Demand|Bioenergy|Overproduction",      detail = detail, dim=3.1))
    
    out <- summationhelper(round(out,8), sep="++")
    getNames(out) <- paste0(getNames(out)," (EJ/yr)")
  }    
 
  return(out)
}