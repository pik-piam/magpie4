#' @title reportEmissions
#' @description reports GHG emissions
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return GHG emissions as MAgPIE object (Unit: Mt CO2/yr, Mt N2O/yr and Mt CH4/yr)
#' @author Florian Humpenoeder, Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportEmissions(gdx)
#'   }

reportEmissions <- function(gdx) {
    
    x <- NULL
  
  #CO2 annual lowpass=3
  total    <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas", lowpass = 3, cc = TRUE, sum=FALSE))
  lu_tot   <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, cc = FALSE, sum=FALSE))
  luc      <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, cc = FALSE, regrowth = FALSE, sum=FALSE))
  
  climatechange <- total-lu_tot
  regrowth <- lu_tot-luc #regrowth is Above Ground Carbon only
  
  #subcategories are only needed for regrowth
  total <- dimSums(total,dim=3)
  climatechange <- dimSums(climatechange,dim=3)
  lu_tot <- dimSums(lu_tot,dim=3)
  luc <- dimSums(luc,dim=3)
  #luc is mostly positive (deforestation), regrowth is mostly negative (regrowth/afforestation). There are, however, some cases that behave differently.
  #luc: cropland-to-pasture conversion causes negative emissions in soilc
  #regrowth: Litter carbon can decrease in case of afforestation/regrowth because the starting level of litter carbon is always pasture litc. If pasture litc is higher than natveg litc, this results in positive emissions.
  
  #Above Ground / Below Ground Carbon
  total_pools <- emisCO2(gdx, level="cell", unit = "gas", pools_aggr=FALSE, lowpass = 3, cc = TRUE)
  lu_pools    <- emisCO2(gdx, level="cell", unit = "gas", pools_aggr=FALSE, lowpass = 3, cc = FALSE)
  climate_pools <- total_pools - lu_pools 
  
  #wood products
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    if(max(readGDX(gdx,"ov_prod")[,,"level"][,,readGDX(gdx,"kforestry")])>0){
      emis_wood_products <- carbonHWP(gdx,unit = "gas")
      #    a <- a - collapseNames(emis_wood_products[,,"wood"])
      wood <- collapseNames(emis_wood_products[,,"ind_rw_pool"]) + collapseNames(emis_wood_products[,,"slow_release_pool"])
      total <- total - wood
      lu_tot <- lu_tot - wood 
    } else { 
      cat("No emission adjustment for carbonHWP in MAgPIE run without timber demand") 
      wood <- total
      wood[,,] <- 0
      }
  } else {
    wood <- total
    wood[,,] <- 0
  }
  
  total         <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_tot        <- mbind(superAggregate(lu_tot,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_tot,level="glo",aggr_type = "sum",na.rm = FALSE))
  luc           <- mbind(superAggregate(luc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(luc,level="glo",aggr_type = "sum",na.rm = FALSE))
  regrowth      <- mbind(superAggregate(regrowth,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(regrowth,level="glo",aggr_type = "sum",na.rm = FALSE))
  wood          <- mbind(superAggregate(wood,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(wood,level="glo",aggr_type = "sum",na.rm = FALSE))
  climatechange <- mbind(superAggregate(climatechange,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(climatechange,level="glo",aggr_type = "sum",na.rm = FALSE))
  total_pools   <- mbind(superAggregate(total_pools,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total_pools,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_pools      <- mbind(superAggregate(lu_pools,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_pools,level="glo",aggr_type = "sum",na.rm = FALSE))
  climate_pools <- mbind(superAggregate(climate_pools,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(climate_pools,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land (Mt CO2/yr)"))
  x <- mbind(x,setNames(lu_tot,"Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(luc,"Emissions|CO2|Land|Land-use Change|+|LUC without Regrowth (Mt CO2/yr)")) #land-use change
  x <- mbind(x,setNames(dimSums(regrowth,dim=3),"Emissions|CO2|Land|Land-use Change|+|Regrowth (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(dimSums(regrowth[,,"forestry_aff"],dim=3.2),"Emissions|CO2|Land|Land-use Change|Regrowth|CO2-price AR (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(dimSums(regrowth[,,"forestry_ndc"],dim=3.2),"Emissions|CO2|Land|Land-use Change|Regrowth|NPI_NDC AR (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(dimSums(regrowth[,,"forestry_plant"],dim=3.2),"Emissions|CO2|Land|Land-use Change|Regrowth|Timber Plantations (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(dimSums(regrowth[,,c("forestry_aff","forestry_ndc","forestry_plant"),invert=TRUE],dim=3),"Emissions|CO2|Land|Land-use Change|Regrowth|Other (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(wood,"Emissions|CO2|Land|Land-use Change|+|Wood products (Mt CO2/yr)")) #wood products
  x <- mbind(x,setNames(climatechange,"Emissions|CO2|Land|+|Climate Change (Mt CO2/yr)")) #emissions from the terrestrial biosphere
  x <- mbind(x,setNames(total_pools,paste0("Emissions|CO2|Land|++|",getNames(total_pools), " (Mt CO2/yr)"))) #emissions from the terrestrial biosphere
  x <- mbind(x,setNames(lu_pools,paste0("Emissions|CO2|Land|Land-use Change|++|",getNames(lu_pools), " (Mt CO2/yr)"))) #emissions from the terrestrial biosphere
  x <- mbind(x,setNames(climate_pools,paste0("Emissions|CO2|Land|Climate Change|++|",getNames(climate_pools), " (Mt CO2/yr)"))) #emissions from the terrestrial biosphere
  
  #CO2 annual lowpass=0
  total <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 0, cc = TRUE)
  lu_tot <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 0, cc = FALSE)
  luc <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 0, cc = FALSE, regrowth = FALSE)
  
  climatechange <- total-lu_tot
  regrowth <- lu_tot-luc
  
  total <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_tot <- mbind(superAggregate(lu_tot,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_tot,level="glo",aggr_type = "sum",na.rm = FALSE))
  luc <- mbind(superAggregate(luc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(luc,level="glo",aggr_type = "sum",na.rm = FALSE))
  regrowth <- mbind(superAggregate(regrowth,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(regrowth,level="glo",aggr_type = "sum",na.rm = FALSE))
  climatechange <- mbind(superAggregate(climatechange,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(climatechange,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land RAW (Mt CO2/yr)"))
  x <- mbind(x,setNames(lu_tot,"Emissions|CO2|Land|+|Land-use Change RAW (Mt CO2/yr)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(climatechange,"Emissions|CO2|Land|+|Climate Change RAW (Mt CO2/yr)")) #emissions from the terrestrial biosphere
  
  #CO2 cumulative lowpass=3
  total <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas", lowpass = 3, cumulative = TRUE, cc = TRUE, sum=FALSE)/1000)
  lu_tot <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, cumulative = TRUE, cc = FALSE, sum=FALSE)/1000)
  luc <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, cumulative = TRUE, cc = FALSE, regrowth = FALSE, sum=FALSE)/1000)
  
  climatechange <- total-lu_tot
  regrowth <- lu_tot-luc #regrowth is Above Ground Carbon only.
  #subcategories are only needed for regrowth
  total <- dimSums(total,dim=3)
  climatechange <- dimSums(climatechange,dim=3)
  lu_tot <- dimSums(lu_tot,dim=3)
  luc <- dimSums(luc,dim=3)
  
  total <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_tot <- mbind(superAggregate(lu_tot,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_tot,level="glo",aggr_type = "sum",na.rm = FALSE))
  luc <- mbind(superAggregate(luc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(luc,level="glo",aggr_type = "sum",na.rm = FALSE))
  regrowth <- mbind(superAggregate(regrowth,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(regrowth,level="glo",aggr_type = "sum",na.rm = FALSE))
  climatechange <- mbind(superAggregate(climatechange,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(climatechange,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land|Cumulative (Gt CO2)"))
  x <- mbind(x,setNames(lu_tot,"Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(luc,"Emissions|CO2|Land|Cumulative|Land-use Change|+|LUC without Regrowth (Gt CO2)")) #land-use change
  x <- mbind(x,setNames(dimSums(regrowth,dim=3),"Emissions|CO2|Land|Cumulative|Land-use Change|+|Regrowth (Gt CO2)")) #regrowth of vegetation
  x <- mbind(x,setNames(dimSums(regrowth[,,"forestry_aff"],dim=3.2),"Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|CO2-price AR (Gt CO2)")) #regrowth of vegetation
  x <- mbind(x,setNames(dimSums(regrowth[,,"forestry_ndc"],dim=3.2),"Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|NPI_NDC AR (Gt CO2)")) #regrowth of vegetation
  x <- mbind(x,setNames(dimSums(regrowth[,,"forestry_plant"],dim=3.2),"Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Timber Plantations (Gt CO2)")) #regrowth of vegetation
  x <- mbind(x,setNames(dimSums(regrowth[,,c("forestry_aff","forestry_ndc","forestry_plant"),invert=TRUE],dim=3),"Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Other (Gt CO2)")) #regrowth of vegetation
  x <- mbind(x,setNames(climatechange,"Emissions|CO2|Land|Cumulative|+|Climate Change (Gt CO2)")) #emissions from the terrestrial biosphere
  
  #N2O, NOx, NH3
  n_emissions=c("n2o_n","nh3_n","no2_n","no3_n")
  total <- Emissions(gdx,level="regglo",type=n_emissions,unit="gas",subcategories=TRUE)
  for (emi in getNames(total,dim=2)){
    prefix<-paste0("Emissions|",reportingnames(emi),"|Land")
    a<-total[,,emi]
    emi2=reportingnames(emi)
    x <- mbind(x,setNames(dimSums(a,dim=3),
                          paste0(prefix,"|+|Agriculture (Mt ",emi2,"/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,"awms"],dim=3),
                          paste0(prefix,"|Agriculture|+|Animal Waste Management (Mt ",emi2,"/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("inorg_fert","man_crop","resid","SOM","rice","man_past")],dim=3),
                          paste0(prefix,"|Agriculture|+|Agricultural Soils (Mt ",emi2,"/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("inorg_fert","rice")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Inorganic Fertilizers (Mt ",emi2,"/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("man_crop")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Manure applied to Croplands (Mt ",emi2,"/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("resid")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Decay of Crop Residues (Mt ",emi2,"/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("SOM")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss (Mt ",emi2,"/yr)")))
    #  x <- mbind(x,setNames(dimSums(a[,,c("rice")],dim=3),
    #                     paste0(prefix,"|Agriculture|Agricultural Soils|+|Lower N2O emissions of rice (Mt ",emi2,"/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("man_past")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Pasture (Mt ",emi2,"/yr)")))
  }

  #CH4
  a <- collapseNames(Emissions(gdx,level="regglo",type="ch4",unit="gas",subcategories=TRUE),collapsedim = 2)
  x <- mbind(x,setNames(dimSums(a,dim=3),"Emissions|CH4|Land|+|Agriculture (Mt CH4/yr)"))
  x <- mbind(x,setNames(dimSums(a[,,c("rice")],dim=3),"Emissions|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)"))
  x <- mbind(x,setNames(dimSums(a[,,c("awms")],dim=3),"Emissions|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)"))
  x <- mbind(x,setNames(dimSums(a[,,c("ent_ferm")],dim=3),"Emissions|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)"))
  
  return(x)
}

