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
  
  #CO2 annual
  total <- emisCO2(gdx,level = "cell",unit="gas",cc = TRUE)

  lu <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE)

  cc <- total - lu
  lu_pos <- lu_neg <- lu
  lu_pos[lu_pos < 0] = 0
  lu_neg[lu_neg > 0] = 0
  if(!identical(lu_pos+lu_neg,lu)) warning("Land-use change emission sub-categories (positive and negative) do not add up to total")
  
  total <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu <- mbind(superAggregate(lu,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_pos <- mbind(superAggregate(lu_pos,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_pos,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_neg <- mbind(superAggregate(lu_neg,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_neg,level="glo",aggr_type = "sum",na.rm = FALSE))
  cc <- mbind(superAggregate(cc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(cc,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land (Mt CO2/yr)"))
  x <- mbind(x,setNames(lu,"Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(lu_pos,"Emissions|CO2|Land|Land-use Change|+|Positive (Mt CO2/yr)")) #land-use change
  x <- mbind(x,setNames(lu_neg,"Emissions|CO2|Land|Land-use Change|+|Negative (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(cc,"Emissions|CO2|Land|+|Climate Change (Mt CO2/yr)")) #emissions from the terrestrial biosphere

  #CO2 annual lowpass=1
  total <- emisCO2(gdx,level = "cell",unit="gas",cc = TRUE,lowpass = 1)
  lu <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE,lowpass = 1)
  cc <- total - lu
  lu_pos <- lu_neg <- lu
  lu_pos[lu_pos < 0] = 0
  lu_neg[lu_neg > 0] = 0
  if(!identical(lu_pos+lu_neg,lu)) warning("Land-use change emission sub-categories (positive and negative) do not add up to total")
  
  total <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu <- mbind(superAggregate(lu,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_pos <- mbind(superAggregate(lu_pos,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_pos,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_neg <- mbind(superAggregate(lu_neg,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_neg,level="glo",aggr_type = "sum",na.rm = FALSE))
  cc <- mbind(superAggregate(cc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(cc,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land lowpass=1 (Mt CO2/yr)"))
  x <- mbind(x,setNames(lu,"Emissions|CO2|Land|+|Land-use Change lowpass=1 (Mt CO2/yr)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(lu_pos,"Emissions|CO2|Land|Land-use Change|+|Positive lowpass=1 (Mt CO2/yr)")) #land-use change
  x <- mbind(x,setNames(lu_neg,"Emissions|CO2|Land|Land-use Change|+|Negative lowpass=1 (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(cc,"Emissions|CO2|Land|+|Climate Change lowpass=1 (Mt CO2/yr)")) #emissions from the terrestrial biosphere

  #CO2 annual lowpass=2
  total <- emisCO2(gdx,level = "cell",unit="gas",cc = TRUE,lowpass = 2)
  lu <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE,lowpass = 2)
  cc <- total - lu
  lu_pos <- lu_neg <- lu
  lu_pos[lu_pos < 0] = 0
  lu_neg[lu_neg > 0] = 0
  if(!identical(lu_pos+lu_neg,lu)) warning("Land-use change emission sub-categories (positive and negative) do not add up to total")
  
  total <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu <- mbind(superAggregate(lu,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_pos <- mbind(superAggregate(lu_pos,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_pos,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_neg <- mbind(superAggregate(lu_neg,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_neg,level="glo",aggr_type = "sum",na.rm = FALSE))
  cc <- mbind(superAggregate(cc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(cc,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land lowpass=2 (Mt CO2/yr)"))
  x <- mbind(x,setNames(lu,"Emissions|CO2|Land|+|Land-use Change lowpass=2 (Mt CO2/yr)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(lu_pos,"Emissions|CO2|Land|Land-use Change|+|Positive lowpass=2 (Mt CO2/yr)")) #land-use change
  x <- mbind(x,setNames(lu_neg,"Emissions|CO2|Land|Land-use Change|+|Negative lowpass=2 (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(cc,"Emissions|CO2|Land|+|Climate Change lowpass=2 (Mt CO2/yr)")) #emissions from the terrestrial biosphere
  
  #CO2 annual lowpass=3
  total <- emisCO2(gdx,level = "cell",unit="gas",cc = TRUE,lowpass = 3)
  lu <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE,lowpass = 3)
  cc <- total - lu
  lu_pos <- lu_neg <- lu
  lu_pos[lu_pos < 0] = 0
  lu_neg[lu_neg > 0] = 0
  if(!identical(lu_pos+lu_neg,lu)) warning("Land-use change emission sub-categories (positive and negative) do not add up to total")
  
  total <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu <- mbind(superAggregate(lu,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_pos <- mbind(superAggregate(lu_pos,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_pos,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_neg <- mbind(superAggregate(lu_neg,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_neg,level="glo",aggr_type = "sum",na.rm = FALSE))
  cc <- mbind(superAggregate(cc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(cc,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land lowpass=3 (Mt CO2/yr)"))
  x <- mbind(x,setNames(lu,"Emissions|CO2|Land|+|Land-use Change lowpass=3 (Mt CO2/yr)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(lu_pos,"Emissions|CO2|Land|Land-use Change|+|Positive lowpass=3 (Mt CO2/yr)")) #land-use change
  x <- mbind(x,setNames(lu_neg,"Emissions|CO2|Land|Land-use Change|+|Negative lowpass=3 (Mt CO2/yr)")) #regrowth of vegetation
  x <- mbind(x,setNames(cc,"Emissions|CO2|Land|+|Climate Change lowpass=3 (Mt CO2/yr)")) #emissions from the terrestrial biosphere
  
  #CO2 cumulative
  total <- emisCO2(gdx,level = "cell",unit="gas",cc = TRUE,cumulative = TRUE)/1000
  lu <- emisCO2(gdx,level = "cell",unit="gas",cc = FALSE,cumulative = TRUE)/1000
  cc <- total - lu
  lu_pos <- lu_neg <- lu
  lu_pos[lu_pos < 0] = 0
  lu_neg[lu_neg > 0] = 0
  if(!identical(lu_pos+lu_neg,lu)) warning("Land-use change emission sub-categories (positive and negative) do not add up to total")
  
  total <- mbind(superAggregate(total,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(total,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu <- mbind(superAggregate(lu,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_pos <- mbind(superAggregate(lu_pos,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_pos,level="glo",aggr_type = "sum",na.rm = FALSE))
  lu_neg <- mbind(superAggregate(lu_neg,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(lu_neg,level="glo",aggr_type = "sum",na.rm = FALSE))
  cc <- mbind(superAggregate(cc,level="reg",aggr_type = "sum",na.rm = FALSE),superAggregate(cc,level="glo",aggr_type = "sum",na.rm = FALSE))
  
  x <- mbind(x,setNames(total,"Emissions|CO2|Land|Cumulative (Gt CO2)"))
  x <- mbind(x,setNames(lu,"Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(lu_pos,"Emissions|CO2|Land|Cumulative|Land-use Change|+|Positive (Gt CO2)")) #land-use change
  x <- mbind(x,setNames(lu_neg,"Emissions|CO2|Land|Cumulative|Land-use Change|+|Negative (Gt CO2)")) #regrowth of vegetation
  x <- mbind(x,setNames(cc,"Emissions|CO2|Land|Cumulative|+|Climate Change (Gt CO2)")) #emissions from the terrestrial biosphere
  
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

