#' @title carbonstock
#' @description reads carbon stocks out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param sum_cpool sum over carbon pool dimension (default = TRUE)
#' @param sum_land sum over land type dimension (default = TRUE)
#' @details carbon pools consist of vegetation carbon (vegc), litter carbon (litc) and soil carbon (soilc)
#' @return carbon stocks in MtC
#' @author Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass dimSums collapseNames write.magpie
#' @importFrom luscale superAggregate
#' @importFrom utils head
#' @examples
#'
#'   \dontrun{
#'     x <- carbonstock(gdx)
#'   }
#'

carbonstock <- function(gdx, file=NULL, level="cell", sum_cpool=TRUE, sum_land=TRUE){
  
  #read in carbon stocks
  a <- readGDX(gdx,"ov_carbon_stock",select=list(type="level"),react="silent")
  names(dimnames(a))[1] <- "j"
  
  dyn_som <- !is.null(readGDX(gdx, "ov59_som_pool", react="silent"))
  
  #calculate detailed forestry land module carbon stock: aff, ndc, plant
  p32_land <- landForestry(gdx,level = "cell")
  if(!is.null(p32_land)) {
    p32_carbon_density_ac <- readGDX(gdx,"p32_carbon_density_ac",react = "silent")
    if(is.null(p32_carbon_density_ac)) p32_carbon_density_ac <- readGDX(gdx,"pm_carbon_density_ac")
    ov32_carbon_stock <- p32_land*p32_carbon_density_ac
    ov32_carbon_stock <- dimSums(ov32_carbon_stock,dim=3.2)
    if(!"soilc" %in% getNames(ov32_carbon_stock,dim=2)) {
      names(dimnames(ov32_carbon_stock))[[3]] <- "type32.c_pools"
      if(dyn_som) {
        cshare    <- collapseNames(cshare(gdx, level="cell", noncrop_aggr=FALSE, reference="actual")[,,"forestry"])
        cshare[is.na(cshare)]     <- 1
        top <- readGDX(gdx, "f59_topsoilc_density")[,getYears(cshare),]
        sub <- readGDX(gdx, "i59_subsoilc_density")[,getYears(cshare),]
        soilc <- dimSums(p32_land,dim=3.2) * (top * cshare + sub)
        soilc <- add_dimension(soilc,dim=3.2,add="c_pools",nm = "soilc")
      } else {
        soilc <- dimSums(p32_land,dim=3.2)*collapseNames(readGDX(gdx,"fm_carbon_density")[,getYears(p32_land),"forestry"])[,,"soilc"]
      }
      ov32_carbon_stock <- mbind(ov32_carbon_stock,soilc)
    }
    #check
    if(abs(sum(dimSums(ov32_carbon_stock,dim=3.1)-collapseNames(a[,,"forestry"]))) > 0.1){
      warning("Differences in ov32_carbon_stock detected!")
    }
    #integrate
    getNames(ov32_carbon_stock,dim=1) <- paste("forestry",getNames(ov32_carbon_stock,dim=1),sep="_")
    a <- a[,,"forestry",invert=TRUE]
    a <- mbind(a,ov32_carbon_stock)
  }
  
  #rounding
  #a <- round(a,digits = 3)
  
  #sum over land pools
  if (sum_land) a <- dimSums(a,dim="land")
  
  #sum over carbon pools
  if (sum_cpool) a <- dimSums(a,dim="c_pools")
  
  #aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  
  out(a,file)
}
