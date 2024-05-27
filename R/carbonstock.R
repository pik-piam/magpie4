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
#' @param stockType carbon stock type (default = "actual"). Options: "actual", "previousLandPattern" and "previousCarbonDensity".
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

carbonstock <- function(gdx, file=NULL, level="cell", sum_cpool=TRUE, sum_land=TRUE, stockType="actual"){

  #read in carbon stocks
  a <- readGDX(gdx,"ov_carbon_stock",select=list(type="level"),react="silent")
  names(dimnames(a))[1] <- "j"
  if(length(getDim(stockType,a)) > 0) {
    a <- collapseNames(a[,,stockType])
  }

  dyn_som <- !is.null(readGDX(gdx, "ov59_som_pool", react="silent"))

  #calculate detailed other land carbon stock: othernat and youngsecdf
  ov_land_other <- readGDX(gdx, "ov_land_other", select = list(type = "level"), react = "silent")
  if(!is.null(ov_land_other)) {
    p35_carbon_density_other <- readGDX(gdx, "p35_carbon_density_other")
    other_carbon_stock <- ov_land_other*p35_carbon_density_other
    other_carbon_stock <- dimSums(other_carbon_stock,dim="ac")
    if(!"soilc" %in% getNames(other_carbon_stock,dim="ag_pools")) {
      names(dimnames(other_carbon_stock))[[3]] <- "land.c_pools"
      if(dyn_som) {
        ov59_som_pool <- readGDX(gdx, "ov59_som_pool", select = list(type = "level"))
        ov_land <- readGDX(gdx, "ov_land", select = list(type = "level"))
        top <- ov59_som_pool / ov_land
        top[is.na(top)] <- 0
        top[is.infinite(top)] <- 0
        sub <- readGDX(gdx, "i59_subsoilc_density")[,getYears(top),]
        soilc <- dimSums(ov_land_other,dim="ac") * (collapseNames(top[,,"other"]) + sub)
        soilc <- add_dimension(soilc,dim=3.2,add="c_pools",nm = "soilc")
      } else {
        soilc1 <- dimSums(ov_land_other[,,"othernat"],dim="ac")*collapseNames(readGDX(gdx,"fm_carbon_density")[,getYears(other_carbon_stock),"other"])[,,"soilc"]
        soilc2 <- dimSums(ov_land_other[,,"youngsecdf"],dim="ac")*collapseNames(readGDX(gdx,"fm_carbon_density")[,getYears(other_carbon_stock),"secdforest"])[,,"soilc"]
        soilc <- mbind(soilc1,soilc2)
      }
      other_carbon_stock <- mbind(other_carbon_stock,soilc)
    }

    #check
    if(abs(sum(dimSums(other_carbon_stock,dim=3.1)-collapseNames(a[,,"other"]))) > 0.1){
      warning("Differences in other land carbon stock detected!")
    }
    #integrate
    a <- a[,,"other",invert=TRUE]
    a <- mbind(a,other_carbon_stock)
  }


  #calculate detailed forestry land module carbon stock: aff, ndc, plant
  p32_land <- landForestry(gdx,level = "cell")
  if(!is.null(p32_land)) {
    p32_carbon_density_ac <- readGDX(gdx,"p32_carbon_density_ac",react = "silent")
    if(is.null(p32_carbon_density_ac)) p32_carbon_density_ac <- readGDX(gdx,"pm_carbon_density_ac")
    ov32_carbon_stock <- p32_land*p32_carbon_density_ac
    ov32_carbon_stock <- dimSums(ov32_carbon_stock,dim="ac")
    if(!"soilc" %in% getNames(ov32_carbon_stock,dim="ag_pools")) {
      names(dimnames(ov32_carbon_stock))[[3]] <- "land.c_pools"
      if(dyn_som) {
        top <- ov59_som_pool / ov_land
        top[is.na(top)] <- 0
        top[is.infinite(top)] <- 0
        sub <- readGDX(gdx, "i59_subsoilc_density")[,getYears(top),]
        soilc <- dimSums(p32_land,dim=3.2) * (collapseNames(top[,,"forestry"]) + sub)
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
