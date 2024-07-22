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
#' @param subcategories NULL or vector of strings. If NULL, no subcategories are returned. Meaningful options
#'  are "crop, "forestry" and "other"
#' @param stockType carbon stock type (default = "actual"). Options: "actual", "previousLandPattern" and "previousCarbonDensity".
#' @details carbon pools consist of vegetation carbon (vegc), litter carbon (litc) and soil carbon (soilc)
#' @return carbon stocks in MtC
#' @author Florian Humpenoeder
#' @importFrom magclass dimSums collapseNames write.magpie
#' @importFrom luscale superAggregate
#' @importFrom utils head
#' @examples
#'
#'   \dontrun{
#'     x <- carbonstock(gdx)
#'   }
#'

carbonstock <- function(gdx, file=NULL, level="cell", sum_cpool=TRUE, sum_land=TRUE, subcategories = NULL, stockType="actual"){

  #read in carbon stocks
  a <- readGDX(gdx,"ov_carbon_stock",select=list(type="level"),react="silent")
  names(dimnames(a))[1] <- "j"
  if(length(getDim(stockType,a)) > 0) {
    a <- collapseNames(a[,,stockType])
  }

  dyn_som <- !is.null(readGDX(gdx, "ov59_som_pool", react="silent"))

  if (!is.null(subcategories)) {
    if ("crop" %in% subcategories) {
      croparea_land <- readGDX(gdx, "ov_area", select = list(type = "level"))
      fallow_land <- readGDX(gdx, "ov_fallow", select = list(type = "level"), react = "silent")
      croptree_land <- readGDX(gdx, "ov29_treecover", select = list(type = "level"), react = "silent")
      if(!is.null(fallow_land) && !is.null(croptree_land)) {

        p29_carbon_density_ac <- readGDX(gdx, "p29_carbon_density_ac")
        fm_carbon_density <- collapseNames(readGDX(gdx, "fm_carbon_density")[,getYears(p29_carbon_density_ac),getNames(p29_carbon_density_ac,dim=2)][,,"crop"])
        names(dimnames(fm_carbon_density))[[3]] <- "ag_pools"

        croparea_carbon_stock <- dimSums(croparea_land, dim = 3) * fm_carbon_density
        names(dimnames(croparea_carbon_stock))[[3]] <- "ag_pools"
        croparea_carbon_stock <- add_dimension(croparea_carbon_stock, dim = 3.1, add = "land", "area")

        fallow_carbon_stock <- fallow_land * fm_carbon_density
        fallow_carbon_stock <- add_dimension(fallow_carbon_stock, dim = 3.1, add = "land", "fallow")

        croptree_carbon_stock <- dimSums(croptree_land * p29_carbon_density_ac, dim = "ac")
        croptree_carbon_stock <- add_dimension(croptree_carbon_stock, dim = 3.1, add = "land", "treecover")

        crop_carbon_stock <- mbind(croparea_carbon_stock,fallow_carbon_stock,croptree_carbon_stock)

        if(!"soilc" %in% getNames(crop_carbon_stock,dim="ag_pools")) {
          names(dimnames(crop_carbon_stock))[[3]] <- "land.c_pools"
          if(dyn_som) {
            # compose crop areas
            crop <- mbind(add_dimension(dimSums(croparea_land, dim = 3), dim = 3.1, add = "land", "area"),
                          add_dimension(fallow_land, dim = 3.1, add = "land", "fallow"),
                          add_dimension(dimSums(croptree_land, dim = "ac"), dim = 3.1, add = "land", "treecover"))

            # recalculate ov59_som_pool for checking
            ov59_som_pool_check <- readGDX(gdx, "ov59_som_pool", select = list(type = "level"))

            ov59_som_target <- readGDX(gdx, "ov59_som_target", select = list(type = "level"))
            i59_lossrate <- readGDX(gdx,"i59_lossrate")
            p59_carbon_density <- readGDX(gdx,"p59_carbon_density")[,getYears(i59_lossrate),]
            names(dimnames(p59_carbon_density))[[3]] <- "land_from"
            names(dimnames(p59_carbon_density))[[2]] <- "t"
            ov_lu_transitions <- readGDX(gdx, "ov_lu_transitions", select = list(type = "level"))
            names(dimnames(ov_lu_transitions))[[3]] <- "land_from.land"
            ov59_som_pool_intermediate <- (1 - i59_lossrate) * dimSums(p59_carbon_density * ov_lu_transitions, dim="land_from")

            ov59_som_pool <- ov59_som_target * i59_lossrate + ov59_som_pool_intermediate

            if(abs(sum(ov59_som_pool - ov59_som_pool_check)) > 1e-6) warning("differences in ov59_som_pool detected")

            # split crop som pool based with crop (area, fallow, treecover) as weight
            w <- crop / dimSums(crop, dim=3)
            w[is.na(w)] <- 1/3
            ov59_som_pool_intermediate <- collapseNames(ov59_som_pool_intermediate[,,"crop"]) * w

            # recalculate ov59_som_target for area, fallow and treecover
            zz <- ov59_som_pool_intermediate
            zz[,,] <- 0
            zz[,,"area"] <- dimSums(croparea_land * readGDX(gdx,"i59_cratio"), dim=3) * readGDX(gdx,"f59_topsoilc_density")[,getYears(zz),]
            zz[,,"fallow"] <- fallow_land * readGDX(gdx,"i59_cratio_fallow") * readGDX(gdx,"f59_topsoilc_density")[,getYears(zz),]
            zz[,,"treecover"] <- dimSums(croptree_land, dim = "ac") * readGDX(gdx,"i59_cratio_treecover") * readGDX(gdx,"f59_topsoilc_density")[,getYears(zz),]
            if(abs(sum(dimSums(zz,dim=3) - collapseNames(ov59_som_target[,,"crop"]))) > 1e-6) warning("differences in ov59_som_target detected")
            ov59_som_target <- zz

            # recalculate ov59_som_pool for area, fallow and treecover
            ov59_som_pool <- ov59_som_target * i59_lossrate + ov59_som_pool_intermediate

            # derive top soil density for area, fallow and treecover
            top <- ov59_som_pool / crop
            top[is.na(top)] <- 0
            top[is.infinite(top)] <- 0
            sub <- readGDX(gdx, "i59_subsoilc_density")[,getYears(top),]
            soilc <- crop * (top + sub)
            soilc <- add_dimension(soilc,dim=3.2,add="c_pools",nm = "soilc")
          } else {
            soilc_croparea <- dimSums(croparea_land, dim = 3) * (readGDX(gdx,"i59_topsoilc_density")[,getYears(crop_carbon_stock),] + readGDX(gdx,"i59_subsoilc_density")[,getYears(crop_carbon_stock),])
            soilc_croparea <- add_dimension(soilc_croparea, dim = 3.1, add = "land", "area")
            soilc_fallow <- fallow_land * (readGDX(gdx,"i59_topsoilc_density")[,getYears(crop_carbon_stock),] + readGDX(gdx,"i59_subsoilc_density")[,getYears(crop_carbon_stock),])
            soilc_fallow <- add_dimension(soilc_fallow, dim = 3.1, add = "land", "fallow")
            soilc_croptree <- dimSums(croptree_land, dim = "ac") * collapseNames(readGDX(gdx,"fm_carbon_density")[,getYears(crop_carbon_stock),"secdforest"][,,"soilc"])
            soilc_croptree <- add_dimension(soilc_croptree, dim = 3.1, add = "land", "treecover")
            soilc <- mbind(soilc_croparea,soilc_fallow,soilc_croptree)
            soilc <- add_dimension(soilc,dim=3.2,add="c_pools",nm = "soilc")
          }
          crop_carbon_stock <- mbind(crop_carbon_stock,soilc)
        }

        #check
        if(abs(sum(dimSums(crop_carbon_stock,dim=3.1)-collapseNames(a[,,"crop"]))) > 0.1){
          warning("Differences in crop land detected!")
        }
        getNames(crop_carbon_stock,dim=1) <- paste("crop",getNames(crop_carbon_stock,dim=1),sep="_")
        crop <- crop_carbon_stock
      } else {
        a1 <- a[,,"crop"]
        getNames(a1,dim=1) <- "area"
        a2 <- a1
        a2[,,] <- 0
        getNames(a2,dim=1) <- "fallow"
        a3 <- a1
        a3[,,] <- 0
        getNames(a3,dim=1) <- "treecover"
        crop <- mbind(a1,a2,a3)
        getNames(crop,dim=1) <- paste("crop",getNames(crop,dim=1),sep="_")
      }
    } else {
      crop <- a[, , "crop"]
    }
    if ("past" %in% subcategories) {
      warning("There are no subcatgories for pasture. Returning total pasture area")
      past <- a[, , "past"]
    } else {
      past <- a[, , "past"]
    }
    if ("forestry" %in% subcategories) {
      #calculate detailed forestry land module carbon stock: aff, ndc, plant
      ov32_land <- readGDX(gdx, "ov32_land", select = list(type = "level"))
      if(!is.null(ov32_land)) {
        p32_carbon_density_ac <- readGDX(gdx,"p32_carbon_density_ac",react = "silent")
        if(is.null(p32_carbon_density_ac)) p32_carbon_density_ac <- readGDX(gdx,"pm_carbon_density_ac")
        ov32_carbon_stock <- ov32_land*p32_carbon_density_ac
        ov32_carbon_stock <- dimSums(ov32_carbon_stock,dim="ac")
        if(!"soilc" %in% getNames(ov32_carbon_stock,dim="ag_pools")) {
          names(dimnames(ov32_carbon_stock))[[3]] <- "land.c_pools"
          if(dyn_som) {
            ov59_som_pool <- readGDX(gdx, "ov59_som_pool", select = list(type = "level"))
            ov_land <- readGDX(gdx, "ov_land", select = list(type = "level"))
            top <- ov59_som_pool / ov_land
            top[is.na(top)] <- 0
            top[is.infinite(top)] <- 0
            sub <- readGDX(gdx, "i59_subsoilc_density")[,getYears(top),]
            soilc <- dimSums(ov32_land,dim=3.2) * (collapseNames(top[,,"forestry"]) + sub)
            soilc <- add_dimension(soilc,dim=3.2,add="c_pools",nm = "soilc")
          } else {
            soilc <- dimSums(ov32_land,dim=3.2)*collapseNames(readGDX(gdx,"fm_carbon_density")[,getYears(ov32_land),"forestry"])[,,"soilc"]
          }
          ov32_carbon_stock <- mbind(ov32_carbon_stock,soilc)
        }
        #check
        if(abs(sum(dimSums(ov32_carbon_stock,dim=3.1)-collapseNames(a[,,"forestry"]))) > 0.1){
          warning("Differences in ov32_carbon_stock detected!")
        }
        getNames(ov32_carbon_stock,dim=1) <- paste("forestry",getNames(ov32_carbon_stock,dim=1),sep="_")
        forestry <- ov32_carbon_stock
      }
    } else {
      forestry <- a[, , "forestry"]
    }
    if ("primforest" %in% subcategories) {
      warning("There are no subcatgories for primforest Returning total primforest area")
      primforest <- a[, , "primforest"]
    } else {
      primforest <- a[, , "primforest"]
    }
    if ("secdforest" %in% subcategories) {
      warning("There are no subcatgories for secdforest. Returning total secdforest area")
      secdforest <- a[, , "secdforest"]
    } else {
      secdforest <- a[, , "secdforest"]
    }
    if ("urban" %in% subcategories) {
      warning("There are no subcatgories for urban land. Returning total urban area")
      urban <- a[, , "urban"]
    } else {
      urban <- a[, , "urban"]
    }
    if ("other" %in% subcategories) {
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
        getNames(other_carbon_stock,dim=1) <- paste("other",getNames(other_carbon_stock,dim=1),sep="_")
        other <- other_carbon_stock
      } else {
        a1 <- a[,,"other"]
        getNames(a1,dim=1) <- "othernat"
        a2 <- a1
        a2[,,] <- 0
        getNames(a2,dim=1) <- "youngsecdf"
        other <- mbind(a1,a2)
        getNames(other,dim=1) <- paste("other",getNames(other,dim=1),sep="_")
      }
    } else {
      other <- a[, , "other"]
    }
    a <- mbind(crop, past, forestry, primforest, secdforest, urban, other)
  }

  #sum over land pools
  if (sum_land) a <- dimSums(a,dim="land")

  #sum over carbon pools
  if (sum_cpool) a <- dimSums(a,dim="c_pools")

  #aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)

  out(a,file)
}
