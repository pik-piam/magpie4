#' @title reportEmissions
#' @description reports GHG emissions
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param storage Accounting for long term carbon storage in wood products. Default is TRUE
#' @return GHG emissions as MAgPIE object (Unit: Mt CO2/yr, Mt N2O/yr and Mt CH4/yr)
#' @author Florian Humpenoeder, Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportEmissions(gdx)
#'   }

reportEmissions <- function(gdx, storage = TRUE) {
    
  # read in carbonstocks first for better performance (instead of repeatingly reading them in via emisCO2)
  stock_cc_rg     <- carbonstock(gdx, level="cell", sum_cpool = FALSE, sum_land = FALSE, cc = TRUE,  regrowth = TRUE)
  stock_nocc_rg   <- carbonstock(gdx, level="cell", sum_cpool = FALSE, sum_land = FALSE, cc = FALSE, regrowth = TRUE)
  stock_nocc_norg <- carbonstock(gdx, level="cell", sum_cpool = FALSE, sum_land = FALSE, cc = FALSE, regrowth = FALSE)
  
  #CO2 annual lowpass=3
  total    <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas", lowpass = 3, stock=stock_cc_rg, sum=FALSE))
  lu_tot   <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, stock=stock_nocc_rg, sum=FALSE))
  luc      <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, stock=stock_nocc_norg, sum=FALSE))
  
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
  total_pools <- emisCO2(gdx, level="cell", unit = "gas", pools_aggr=FALSE, lowpass = 3, stock=stock_cc_rg)
  lu_pools    <- emisCO2(gdx, level="cell", unit = "gas", pools_aggr=FALSE, lowpass = 3, stock=stock_nocc_rg)
  climate_pools <- total_pools - lu_pools 
  
  #wood products
  emis_wood_products <- carbonLTS(gdx,unit = "gas")
  if(!is.null(emis_wood_products) && storage) {
    #calc storage and decay
    wood_storage <- -collapseNames(emis_wood_products[,,"storage"][,,"wood"])
    wood_decay <- collapseNames(emis_wood_products[,,"decay"][,,"wood"])
    wood <- wood_storage + wood_decay
    #recalculate top categories
    lu_tot <- luc + dimSums(regrowth, dim=3) + wood
    total <- lu_tot + climatechange
    #check
    if (abs(sum(total-(lu_tot+climatechange),na.rm=TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    if (abs(sum(lu_tot-(luc+dimSums(regrowth,dim=3)+collapseNames(wood)),na.rm=TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    #assign proper names
    getNames(wood) <- "Emissions|CO2|Land|Land-use Change|+|Wood products (Mt CO2/yr)" #carbon stored in wood products + release from wood products
    getNames(wood_storage) <- "Emissions|CO2|Land|Land-use Change|Wood products|+|Storage (Mt CO2/yr)" #carbon stored in wood products
    getNames(wood_decay) <- "Emissions|CO2|Land|Land-use Change|Wood products|+|Release (Mt CO2/yr)" #slow release from wood products
  } else wood <- wood_storage <- wood_decay <- NULL

  peatland <- PeatlandEmissions(gdx,unit="gas",lowpass = 3)
  if(!is.null(peatland)) {
    peatland <- collapseNames(peatland[,,"co2"])
    total <- total + peatland
    getNames(peatland) <- "Emissions|CO2|Land|+|Peatland (Mt CO2/yr)"
  }
  
  x <- mbind(setNames(total,"Emissions|CO2|Land (Mt CO2/yr)"),
             peatland,
             setNames(lu_tot,"Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"), #includes land-use change and regrowth of vegetation
             setNames(luc,"Emissions|CO2|Land|Land-use Change|+|Gross LUC (Mt CO2/yr)"), #land-use change
             setNames(dimSums(regrowth,dim=3),"Emissions|CO2|Land|Land-use Change|+|Regrowth (Mt CO2/yr)"), #regrowth of vegetation
             setNames(dimSums(regrowth[,,"forestry_aff"],dim=3.2),"Emissions|CO2|Land|Land-use Change|Regrowth|CO2-price AR (Mt CO2/yr)"), #regrowth of vegetation
             setNames(dimSums(regrowth[,,"forestry_ndc"],dim=3.2),"Emissions|CO2|Land|Land-use Change|Regrowth|NPI_NDC AR (Mt CO2/yr)"), #regrowth of vegetation
             setNames(dimSums(regrowth[,,"forestry_plant"],dim=3.2),"Emissions|CO2|Land|Land-use Change|Regrowth|Timber Plantations (Mt CO2/yr)"), #regrowth of vegetation
             setNames(dimSums(regrowth[,,c("forestry_aff","forestry_ndc","forestry_plant"),invert=TRUE],dim=3),"Emissions|CO2|Land|Land-use Change|Regrowth|Other (Mt CO2/yr)"), #regrowth of vegetation
             wood, #wood products
             wood_storage, #carbon stored in wood products
             wood_decay, #slow release from wood products
             setNames(climatechange,"Emissions|CO2|Land|+|Climate Change (Mt CO2/yr)"), #emissions from the terrestrial biosphere
             setNames(total_pools,paste0("Emissions|CO2|Land|++|",getNames(total_pools), " (Mt CO2/yr)")), #emissions from the terrestrial biosphere
             setNames(lu_pools,paste0("Emissions|CO2|Land|Land-use Change|++|",getNames(lu_pools), " (Mt CO2/yr)")), #emissions from the terrestrial biosphere
             setNames(climate_pools,paste0("Emissions|CO2|Land|Climate Change|++|",getNames(climate_pools), " (Mt CO2/yr)"))) #emissions from the terrestrial biosphere

  #CO2 annual lowpass=0
  total  <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 0, stock=stock_cc_rg)
  lu_tot <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 0, stock=stock_nocc_rg)
  luc    <- emisCO2(gdx, level="cell", unit = "gas", lowpass = 0, stock=stock_nocc_norg)
  
  climatechange <- total-lu_tot
  regrowth <- lu_tot-luc
  
  peatland <- PeatlandEmissions(gdx,unit="gas",lowpass = 0)
  if(!is.null(peatland)) {
    peatland <- collapseNames(peatland[,,"co2"])
    total <- total + peatland
    getNames(peatland) <- "Emissions|CO2|Land RAW|+|Peatland (Mt CO2/yr)"
  }
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land RAW (Mt CO2/yr)"),
               peatland,
               setNames(lu_tot,"Emissions|CO2|Land RAW|+|Land-use Change RAW (Mt CO2/yr)"), #includes land-use change and regrowth of vegetation
               setNames(climatechange,"Emissions|CO2|Land RAW|+|Climate Change RAW (Mt CO2/yr)")) #emissions from the terrestrial biosphere

  #CO2 cumulative lowpass=3
  total  <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas", lowpass = 3, cumulative = TRUE, stock=stock_cc_rg, sum=FALSE)/1000)
  lu_tot <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, cumulative = TRUE, stock=stock_nocc_rg, sum=FALSE)/1000)
  luc    <- suppressWarnings(emisCO2(gdx, level="cell", unit = "gas",lowpass = 3, cumulative = TRUE, stock=stock_nocc_norg, sum=FALSE)/1000)
  
  climatechange <- total-lu_tot
  regrowth <- lu_tot-luc #regrowth is Above Ground Carbon only.
  #subcategories are only needed for regrowth
  total <- dimSums(total,dim=3)
  climatechange <- dimSums(climatechange,dim=3)
  lu_tot <- dimSums(lu_tot,dim=3)
  luc <- dimSums(luc,dim=3)
  
  #wood products
  emis_wood_products <- carbonLTS(gdx,unit = "gas",cumulative = TRUE)
  if(!is.null(emis_wood_products) && storage) {
    emis_wood_products <- emis_wood_products/1000
    #calc storage and decay
    wood_storage <- -collapseNames(emis_wood_products[,,"storage"][,,"wood"])
    wood_decay <- collapseNames(emis_wood_products[,,"decay"][,,"wood"])
    wood <- wood_storage + wood_decay
    #recalculate top categories
    lu_tot <- luc + dimSums(regrowth, dim=3) + wood
    total <- lu_tot + climatechange
    #check
    if (abs(sum(total-(lu_tot+climatechange),na.rm=TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    if (abs(sum(lu_tot-(luc+dimSums(regrowth,dim=3)+collapseNames(wood)),na.rm=TRUE)) > 0.1) warning("Emission subcategories do not add up to total! Check the code.")
    #assign proper names
    getNames(wood) <- "Emissions|CO2|Land|Cumulative|Land-use Change|+|Wood products (Gt CO2)" #carbon stored in wood products + release from wood products
    getNames(wood_storage) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood products|+|Storage (Gt CO2)" #carbon stored in wood products
    getNames(wood_decay) <- "Emissions|CO2|Land|Cumulative|Land-use Change|Wood products|+|Release (Gt CO2)" #slow release from wood products
  } else wood <- wood_storage <- wood_decay <- NULL
  
  peatland <- PeatlandEmissions(gdx,unit="gas",cumulative=TRUE,lowpass = 3)
  if(!is.null(peatland)) {
    peatland <- collapseNames(peatland[,,"co2"])/1000
    total <- total + peatland
    getNames(peatland) <- "Emissions|CO2|Land|Cumulative|+|Peatland (Gt CO2)"
  }
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land|Cumulative (Gt CO2)"),
               peatland,
               setNames(lu_tot,"Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)"), #includes land-use change and regrowth of vegetation
               setNames(luc,"Emissions|CO2|Land|Cumulative|Land-use Change|+|Gross LUC (Gt CO2)"), #land-use change
               setNames(dimSums(regrowth,dim=3),"Emissions|CO2|Land|Cumulative|Land-use Change|+|Regrowth (Gt CO2)"), #regrowth of vegetation
               setNames(dimSums(regrowth[,,"forestry_aff"],dim=3.2),"Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|CO2-price AR (Gt CO2)"), #regrowth of vegetation
               setNames(dimSums(regrowth[,,"forestry_ndc"],dim=3.2),"Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|NPI_NDC AR (Gt CO2)"), #regrowth of vegetation
               setNames(dimSums(regrowth[,,"forestry_plant"],dim=3.2),"Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Timber Plantations (Gt CO2)"), #regrowth of vegetation
               setNames(dimSums(regrowth[,,c("forestry_aff","forestry_ndc","forestry_plant"),invert=TRUE],dim=3),"Emissions|CO2|Land|Cumulative|Land-use Change|Regrowth|Other (Gt CO2)"), #regrowth of vegetation
               wood, #wood products
               wood_storage, #carbon stored in wood products
               wood_decay, #slow release from wood products
               setNames(climatechange,"Emissions|CO2|Land|Cumulative|+|Climate Change (Gt CO2)")) #emissions from the terrestrial biosphere
  
  x <- superAggregateX(x, level="regglo", aggr_type = "sum")
  
  #N2O, NOx, NH3
  n_emissions=c("n2o_n","nh3_n","no2_n","no3_n")
  total <- Emissions(gdx,level="regglo",type=n_emissions,unit="gas",subcategories=TRUE)
  
  peatland <- PeatlandEmissions(gdx,unit="gas",level="regglo")
  if(!is.null(peatland)) {
    peatland <- collapseNames(peatland[,,"n2o"])
    getNames(peatland) <- "Emissions|N2O|Land|+|Peatland (Mt N2O/yr)"
    total_n2o <- dimSums(total[,,"n2o"],dim=3) + peatland
    getNames(total_n2o) <- "Emissions|N2O|Land (Mt N2O/yr)"
  } else total_n2o <- NULL
  x <- mbind(x,total_n2o)
  x <- mbind(x,peatland)
  
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
    #               setNames(dimSums(a[,,c("rice")],dim=3),
    #                     paste0(prefix,"|Agriculture|Agricultural Soils|+|Lower N2O emissions of rice (Mt ",emi2,"/yr)")),
                 setNames(dimSums(a[,,c("man_past")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Pasture (Mt ",emi2,"/yr)")))
  }

  #CH4
  a <- collapseNames(Emissions(gdx,level="regglo",type="ch4",unit="gas",subcategories=TRUE),collapsedim = 2)
  
  peatland <- PeatlandEmissions(gdx,unit="gas",level="regglo")
  if(!is.null(peatland)) {
    peatland <- collapseNames(peatland[,,"ch4"])
    getNames(peatland) <- "Emissions|CH4|Land|+|Peatland (Mt CH4/yr)"
    total_ch4 <- dimSums(a,dim=3) + peatland
    getNames(total_ch4) <- "Emissions|CH4|Land (Mt CH4/yr)"
  } else total_ch4 <- NULL
  x <- mbind(x,total_ch4)
  x <- mbind(x,peatland)
  
  x <- mbind(x,setNames(dimSums(a,dim=3),"Emissions|CH4|Land|+|Agriculture (Mt CH4/yr)"),
               setNames(dimSums(a[,,c("rice")],dim=3),"Emissions|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)"),
               setNames(dimSums(a[,,c("awms")],dim=3),"Emissions|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)"),
               setNames(dimSums(a[,,c("ent_ferm")],dim=3),"Emissions|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)"))
  
  #CH4 GWP
  a <- collapseNames(Emissions(gdx,level="regglo",type="ch4",unit="GWP",subcategories=TRUE),collapsedim = 2)
  #todo: add peatland CH4
  x <- mbind(x,setNames(dimSums(a,dim=3),"Emissions|CH4_GWP|Land|+|Agriculture (Mt CO2eq/yr)"),
             setNames(dimSums(a[,,c("rice")],dim=3),"Emissions|CH4_GWP|Land|Agriculture|+|Rice (Mt CO2eq/yr)"),
             setNames(dimSums(a[,,c("awms")],dim=3),"Emissions|CH4_GWP|Land|Agriculture|+|Animal waste management (Mt CO2eq/yr)"),
             setNames(dimSums(a[,,c("ent_ferm")],dim=3),"Emissions|CH4_GWP|Land|Agriculture|+|Enteric fermentation (Mt CO2eq/yr)"))
  
  #CH4 GWP*
  a <- collapseNames(Emissions(gdx,level="regglo",type="ch4",unit="GWP*",subcategories=TRUE),collapsedim = 2)
  #todo: add peatland CH4
  x <- mbind(x,setNames(dimSums(a,dim=3),"Emissions|CH4_GWP*|Land|+|Agriculture (Mt CO2eq/yr)"),
             setNames(dimSums(a[,,c("rice")],dim=3),"Emissions|CH4_GWP*|Land|Agriculture|+|Rice (Mt CO2eq/yr)"),
             setNames(dimSums(a[,,c("awms")],dim=3),"Emissions|CH4_GWP*|Land|Agriculture|+|Animal waste management (Mt CO2eq/yr)"),
             setNames(dimSums(a[,,c("ent_ferm")],dim=3),"Emissions|CH4_GWP*|Land|Agriculture|+|Enteric fermentation (Mt CO2eq/yr)"))
  
  return(x)
}

