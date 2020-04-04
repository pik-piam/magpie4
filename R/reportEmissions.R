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
  
  # read in carbonstocks first for better performance (instead of repeatingly reading them in via emisCO2)
  stock_cc_rg     <- carbonstock(gdx, level="cell", sum_cpool = FALSE, sum_land = FALSE, cc = TRUE,  regrowth = TRUE)
  stock_nocc_rg   <- carbonstock(gdx, level="cell", sum_cpool = FALSE, sum_land = FALSE, cc = FALSE, regrowth = TRUE)
  stock_nocc_norg <- carbonstock(gdx, level="cell", sum_cpool = FALSE, sum_land = FALSE, cc = FALSE, regrowth = FALSE)
  
  #CO2 annual lowpass = 3
  total       <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 3, stock=stock_cc_rg)
  lu_tot      <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 3, stock=stock_nocc_rg)
  luc         <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 3, stock=stock_nocc_norg)
  total_pools <- emisCO2(gdx, level="cell", unit = "gas", pools_aggr=FALSE, lowpass = 3, stock=stock_cc_rg)
  lu_pools    <- emisCO2(gdx, level="cell", unit = "gas", pools_aggr=FALSE, lowpass = 3, stock=stock_nocc_rg)
  
  climatechange <- total - lu_tot
  climate_pools <- total_pools - lu_pools 
  regrowth      <- lu_tot - luc
  
  x <- mbind(setNames(total,"Emissions|CO2|Land (Mt CO2/yr)"),
             setNames(lu_tot,"Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"), #includes land-use change and regrowth of vegetation
             setNames(luc,"Emissions|CO2|Land|Land-use Change|+|Positive (Mt CO2/yr)"), #land-use change
             setNames(regrowth,"Emissions|CO2|Land|Land-use Change|+|Negative (Mt CO2/yr)"), #regrowth of vegetation
             setNames(climatechange,"Emissions|CO2|Land|+|Climate Change (Mt CO2/yr)"), #emissions from the terrestrial biosphere
             setNames(total_pools,paste0("Emissions|CO2|Land|++|",getNames(total_pools), " (Mt CO2/yr)")), #emissions from the terrestrial biosphere
             setNames(lu_pools,paste0("Emissions|CO2|Land|Land-use Change|++|",getNames(lu_pools), " (Mt CO2/yr)")), #emissions from the terrestrial biosphere
             setNames(climate_pools,paste0("Emissions|CO2|Land|Climate Change|++|",getNames(climate_pools), " (Mt CO2/yr)"))) #emissions from the terrestrial biosphere
  
  
  #CO2 annual lowpass=0
  total  <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 0, stock=stock_cc_rg)
  lu_tot <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 0, stock=stock_nocc_rg)
  luc    <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 0, stock=stock_nocc_norg)
  
  climatechange <- total-lu_tot
  regrowth      <- lu_tot-luc
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land RAW (Mt CO2/yr)"),
             setNames(lu_tot,"Emissions|CO2|Land|+|Land-use Change RAW (Mt CO2/yr)"), #includes land-use change and regrowth of vegetation
             setNames(luc,"Emissions|CO2|Land|Land-use Change|+|Positive RAW (Mt CO2/yr)"), #land-use change
             setNames(regrowth,"Emissions|CO2|Land|Land-use Change|+|Negative RAW (Mt CO2/yr)"), #regrowth of vegetation
             setNames(climatechange,"Emissions|CO2|Land|+|Climate Change RAW (Mt CO2/yr)")) #emissions from the terrestrial biosphere
  
  #CO2 cumulative lowpass=3
  total <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 3, cumulative = TRUE, stock=stock_cc_rg)/1000
  lu_tot <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, cumulative = TRUE, stock=stock_nocc_rg)/1000
  luc <- emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, cumulative = TRUE, stock=stock_nocc_norg)/1000
  
  climatechange <- total-lu_tot
  regrowth <- lu_tot-luc
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land|Cumulative (Gt CO2)"),
             setNames(lu_tot,"Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)"), #includes land-use change and regrowth of vegetation
             setNames(luc,"Emissions|CO2|Land|Cumulative|Land-use Change|+|Positive (Gt CO2)"), #land-use change
             setNames(regrowth,"Emissions|CO2|Land|Cumulative|Land-use Change|+|Negative (Gt CO2)"), #regrowth of vegetation
             setNames(climatechange,"Emissions|CO2|Land|Cumulative|+|Climate Change (Gt CO2)")) #emissions from the terrestrial biosphere
  
  x <- mbind(superAggregate(x, level="reg", aggr_type = "sum", na.rm = FALSE),superAggregate(x, level="glo", aggr_type = "sum", na.rm = FALSE))
  
  #N2O, NOx, NH3
  n_emissions=c("n2o_n","nh3_n","no2_n","no3_n")
  total <- Emissions(gdx,level="regglo",type=n_emissions,unit="gas",subcategories=TRUE)
  for (emi in getNames(total,dim=2)){
    prefix<-paste0("Emissions|",reportingnames(emi),"|Land")
    a<-total[,,emi]
    emi2=reportingnames(emi)
    x <- mbind(x,setNames(dimSums(a,dim=3),
                          paste0(prefix,"|+|Agriculture (Mt ",emi2,"/yr)")),
               setNames(dimSums(a[,,"awms"],dim=3),
                          paste0(prefix,"|Agriculture|+|Animal Waste Management (Mt ",emi2,"/yr)")),
               setNames(dimSums(a[,,c("inorg_fert","man_crop","resid","SOM","rice","man_past")],dim=3),
                          paste0(prefix,"|Agriculture|+|Agricultural Soils (Mt ",emi2,"/yr)")),
               setNames(dimSums(a[,,c("inorg_fert","rice")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Inorganic Fertilizers (Mt ",emi2,"/yr)")),
               setNames(dimSums(a[,,c("man_crop")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Manure applied to Croplands (Mt ",emi2,"/yr)")),
               setNames(dimSums(a[,,c("resid")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Decay of Crop Residues (Mt ",emi2,"/yr)")),
               setNames(dimSums(a[,,c("SOM")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss (Mt ",emi2,"/yr)")),
               setNames(dimSums(a[,,c("man_past")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Pasture (Mt ",emi2,"/yr)")))
  }

  #CH4
  a <- collapseNames(Emissions(gdx,level="regglo",type="ch4",unit="gas",subcategories=TRUE),collapsedim = 2)
  x <- mbind(x,setNames(dimSums(a,dim=3),"Emissions|CH4|Land|+|Agriculture (Mt CH4/yr)"),
               setNames(dimSums(a[,,c("rice")],dim=3),"Emissions|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)"),
               setNames(dimSums(a[,,c("awms")],dim=3),"Emissions|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)"),
               setNames(dimSums(a[,,c("ent_ferm")],dim=3),"Emissions|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)"))
  
  return(x)
}

