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
  
  #CO2 annual lowpass=1
  total    <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 1, cc = TRUE)
  lu_tot   <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 1, cc = FALSE)
  luc      <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 1, cc = FALSE, regrowth = FALSE)
  total_pools <- emisCO2(gdx, level="cell", unit = "gas", pools_aggr=FALSE, lowpass = 1, cc = TRUE)
  lu_pools    <- emisCO2(gdx, level="cell", unit = "gas", pools_aggr=FALSE, lowpass = 1, cc = FALSE)
  
  climatechange <- total-lu_tot
  climate_pools <- total_pools - lu_pools 
  regrowth <- lu_tot-luc
  
  total         <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_tot        <- mbind(superAggregate(lu_tot,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_tot,level="glo",aggr_type = "sum",na.rm = FALSE))
  luc           <- mbind(superAggregate(luc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(luc,level="glo",aggr_type = "sum",na.rm = FALSE))
  regrowth      <- mbind(superAggregate(regrowth,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(regrowth,level="glo",aggr_type = "sum",na.rm = FALSE))
  climatechange <- mbind(superAggregate(climatechange,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(climatechange,level="glo",aggr_type = "sum",na.rm = FALSE))
  total_pools   <- mbind(superAggregate(total_pools,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total_pools,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_pools      <- mbind(superAggregate(lu_pools,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_pools,level="glo",aggr_type = "sum",na.rm = FALSE))
  climate_pools <- mbind(superAggregate(climate_pools,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(climate_pools,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land (Mt CO2/yr)"))
  x <- mbind(x,setNames(lu_tot,"Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)")) #includes land-use change and regrowth of vegetation
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    lu_0 <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE,wood_prod_fraction = 0)
    lu_0 <- mbind(superAggregate(lu_0,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_0,level="glo",aggr_type = "sum",na.rm = FALSE))
    lu_025 <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE,wood_prod_fraction = 0.25)
    lu_025 <- mbind(superAggregate(lu_025,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_025,level="glo",aggr_type = "sum",na.rm = FALSE))
    lu_050 <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE,wood_prod_fraction = 0.5)
    lu_050 <- mbind(superAggregate(lu_050,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_050,level="glo",aggr_type = "sum",na.rm = FALSE))
    lu_075 <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE,wood_prod_fraction = 0.75)
    lu_075 <- mbind(superAggregate(lu_075,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_075,level="glo",aggr_type = "sum",na.rm = FALSE))
    lu_100 <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE,wood_prod_fraction = 1)
    lu_100 <- mbind(superAggregate(lu_100,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_100,level="glo",aggr_type = "sum",na.rm = FALSE))

    x <- mbind(x,setNames(lu_0,"Emissions|CO2|Land|+|Land-use Change (0) (Mt CO2/yr)")) #Wood products
    x <- mbind(x,setNames(lu_025,"Emissions|CO2|Land|+|Land-use Change (025) (Mt CO2/yr)")) #Wood products
    x <- mbind(x,setNames(lu_050,"Emissions|CO2|Land|+|Land-use Change (050) (Mt CO2/yr)")) #Wood products
    x <- mbind(x,setNames(lu_075,"Emissions|CO2|Land|+|Land-use Change (075) (Mt CO2/yr)")) #Wood products
    x <- mbind(x,setNames(lu_100,"Emissions|CO2|Land|+|Land-use Change (100) (Mt CO2/yr)")) #Wood products
  }
  x <- mbind(x,setNames(luc,"Emissions|CO2|Land|Land-use Change|+|Positive (Mt CO2/yr)")) #land-use change
  x <- mbind(x,setNames(regrowth,"Emissions|CO2|Land|Land-use Change|+|Negative (Mt CO2/yr)")) #regrowth of vegetation
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
  x <- mbind(x,setNames(luc,"Emissions|CO2|Land|Land-use Change|+|Positive RAW (Mt CO2/yr)")) #land-use change
  x <- mbind(x,setNames(regrowth,"Emissions|CO2|Land|Land-use Change|+|Negative RAW (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(climatechange,"Emissions|CO2|Land|+|Climate Change RAW (Mt CO2/yr)")) #emissions from the terrestrial biosphere
  
  #CO2 cumulative lowpass=1
  total <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 1, cumulative = TRUE, cc = TRUE)/1000
  lu_tot <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 1, cumulative = TRUE, cc = FALSE)/1000
  luc <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 1, cumulative = TRUE, cc = FALSE, regrowth = FALSE)/1000
  
  climatechange <- total-lu_tot
  regrowth <- lu_tot-luc
  
  total <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_tot <- mbind(superAggregate(lu_tot,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_tot,level="glo",aggr_type = "sum",na.rm = FALSE))
  luc <- mbind(superAggregate(luc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(luc,level="glo",aggr_type = "sum",na.rm = FALSE))
  regrowth <- mbind(superAggregate(regrowth,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(regrowth,level="glo",aggr_type = "sum",na.rm = FALSE))
  climatechange <- mbind(superAggregate(climatechange,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(climatechange,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land|Cumulative (Gt CO2)"))
  x <- mbind(x,setNames(lu_tot,"Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(luc,"Emissions|CO2|Land|Cumulative|Land-use Change|+|Positive (Gt CO2)")) #land-use change
  x <- mbind(x,setNames(regrowth,"Emissions|CO2|Land|Cumulative|Land-use Change|+|Negative (Gt CO2)")) #regrowth of vegetation
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

