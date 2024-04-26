#' @title ResidueUsage
#' @description reads Crop Residue Usage out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @param products Selection of products (either by naming products, e.g. "tece", or naming a set,e.g."kcr")
#' @param product_aggr aggregate over products or not. Usually boolean, but here also the value "kres" is allowed, which provides kcr aggregated to kres
#' @param attributes dry matter: Mt ("dm"), gross energy: PJ ("ge"), reactive nitrogen: Mt ("nr"), phosphor: Mt ("p"), potash: Mt ("k"), wet matter: Mt ("wm"). Can also be a vector.
#' @param water_aggr aggregate irrigated and non-irriagted production or not (boolean).
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @return production as MAgPIE object (unit depends on attributes)
#' @author Kristine Karstens, Michael Crawford
#' @seealso \code{\link{ResidueBiomass}}
#' @examples
#'
#'   \dontrun{
#'     x <- ResidueUsage(gdx)
#'   }
#'

ResidueUsage <- function(gdx,level="reg",dir=".",products="kcr",product_aggr=FALSE,attributes="dm",water_aggr=TRUE,spamfiledirectory=""){

  dir <- getDirectory(dir,spamfiledirectory)

  if(!setequal(attributes,"dm")) warning("Calculation based on dry matter attributes.")

  kres     <- findset("kres")
  kcr      <- findset("kcr")
  kcr2kres <- readGDX(gdx,"kres_kcr")

  if(level%in%c("reg","regglo","glo")){
    ResidueBiomass <- collapseNames(readGDX(gdx,"ov_res_biomass_ag")[,,"level"][,,"dm"])

    Recycling_kcr  <- collapseNames(readGDX(gdx,"ov18_res_ag_recycling")[,,"level"][,,"dm"])
    Burn_kcr       <- collapseNames(readGDX(gdx,"ov_res_ag_burn", "ov18_res_ag_burn", format = "first_found")[,,"level"][,,"dm"])
    Removal_kcr    <- collapseNames(readGDX(gdx,"ov18_res_ag_removal")[,,"level"][,,"dm"])

    Feed_kres      <- dimSums(collapseNames(readGDX(gdx,"ov_dem_feed")[,,"level"][,,kres], collapsedim = "type"), dim=3.1)
    Material_kres  <- collapseNames(readGDX(gdx,"ov_dem_material")[,,"level"][,,kres], collapsedim = "type")
    Bioenergy_kres <- collapseNames(readGDX(gdx,"ov_dem_bioen")[,,"level"][,,kres], collapsedim = "type")
    Waste_kres     <- collapseNames(readGDX(gdx,"ov16_dem_waste")[,,"level"][,,kres], collapsedim = "type")
    Balance_kres   <- collapseNames(readGDX(gdx,"f16_domestic_balanceflow")[,getYears(ResidueBiomass),kres])


    if(products=="kres"){

      ResidueBiomass_kres <- luscale::speed_aggregate(ResidueBiomass, kcr2kres, from="kcr", to="kres", dim=3.1, partrel=TRUE)[,,kres]

      Recycling_kres  <- luscale::speed_aggregate(Recycling_kcr, kcr2kres, from="kcr", to="kres", dim=3.1, partrel=TRUE)[,,kres]
      Burn_kres       <- luscale::speed_aggregate(Burn_kcr, kcr2kres, from="kcr", to="kres", dim=3.1, partrel=TRUE)[,,kres]
      Removal_kres    <- luscale::speed_aggregate(Removal_kcr, kcr2kres, from="kcr", to="kres", dim=3.1, partrel=TRUE)[,,kres]


      Usage <- mbind(add_dimension(Feed_kres, add="usage", nm="feed"),
                     add_dimension(Material_kres, add="usage", nm="material"),
                     add_dimension(Bioenergy_kres, add="usage", nm="bioenergy"),
                     add_dimension(Waste_kres, add="usage", nm="waste"),
                     add_dimension(Balance_kres, add="usage", nm="balance"))

      check <- round(dimSums(Removal_kres, dim=3) - dimSums(Usage, dim=3),5)
      if(any(check!=0)) warning("Sum over all residue removal options and residue removal in total are not matching.")

      Usage <- mbind(add_dimension(collapseNames(Recycling_kres), add="usage", nm="recycling"),
                     add_dimension(collapseNames(Burn_kres), add="usage", nm="burn"),
                     Usage)

      check <- round(ResidueBiomass_kres - dimSums(Usage, dim=3.1),5)
      if(any(check!=0)) warning("Sum over all residue usage options and ag-residue biomass in total are not matching.")


      Attributes      <- readGDX(gdx,"fm_attributes")[,,attributes][,,kres]
      Usage           <- Usage * Attributes

    } else if(products=="kcr"){

      Usage_kres <- mbind(add_dimension(Feed_kres, add="usage", nm="feed"),
                     add_dimension(Material_kres, add="usage", nm="material"),
                     add_dimension(Bioenergy_kres, add="usage", nm="bioenergy"),
                     add_dimension(Waste_kres, add="usage", nm="waste"),
                     add_dimension(Balance_kres, add="usage", nm="balance"))

      Usage      <- luscale::speed_aggregate( Usage_kres,kcr2kres, weight=Removal_kcr[,,kcr2kres$kcr], from="kres", to="kcr", dim=3.2)
      names(dimnames(Usage))[3] <- "usage.kcr"
      Usage      <- add_columns(Usage, addnm= c( "sunflower", "oilpalm", "foddr", "begr", "betr"), dim=3.2)
      Usage[is.na(Usage)] <- 0


      check <- round(dimSums(Removal_kcr, dim="kcr") - dimSums(Usage, dim=c("usage","kcr")),5)
      if(any(check!=0)) warning(paste0("Sum over all residue removal options and residue removal in total are not matching. Non-matching in the range of ", range(check)))

      Usage <- mbind(add_dimension(Recycling_kcr, add="usage", nm="recycling"),
                     add_dimension(Burn_kcr, add="usage", nm="burn"),
                     Usage)

      check <- round(ResidueBiomass - dimSums(Usage, dim="usage"),5)
      if(any(check!=0)) warning(paste0("Sum over all residue usage options and ag-residue biomass in total are not matching. Non-matching in the range of ", range(check)))

      Attributes      <- readGDX(gdx,"f18_attributes_residue_ag")[,,attributes][,,kcr]
      Usage           <- Usage * Attributes

    } else {stop(paste0("Product type ",products," unknown."))}

    ### reg, regglo, glo aggregation
    Usage <- gdxAggregate(gdx, Usage, to=level, absolute=TRUE, dir = dir)

  } else {
    Usage          <- ResidueUsage(gdx,level="reg",dir=dir,products=products,product_aggr=product_aggr,attributes=attributes,water_aggr=water_aggr)
    Usage_share    <- Usage/dimSums(Usage, dim="usage")
    Usage_share[is.na(Usage_share)] <- 0
    Usage_share    <- gdxAggregate(gdx, x=Usage_share, to=level, absolute=FALSE, dir=dir)

    ResidueBiomass <- ResidueBiomass(gdx,level=level,dir=dir,products=products,product_aggr=product_aggr,attributes=attributes,water_aggr=water_aggr)

    Usage                             <- ResidueBiomass * Usage_share
    Usage[,,"bg"][,,"recycling"]      <- ResidueBiomass[,,"bg"]
    invert_recycling <- setdiff(getNames(Usage, dim="usage"),"recycling")
    Usage[,,"bg"][,,invert_recycling] <-  0
  }

  return(Usage)
}
