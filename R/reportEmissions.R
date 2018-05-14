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
  total <- emisCO2(gdx,level = "regglo",unit="gas",lowpass = 1,cc = TRUE)
  lu <- emisCO2(gdx,level = "regglo",unit="gas",lowpass = 1,cc = FALSE)
  cc <- total - lu
  x <- mbind(x,setNames(total,"Emissions|CO2|Land (Mt CO2/yr)"))
  x <- mbind(x,setNames(lu,"Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(cc,"Emissions|CO2|Land|+|Climate Change (Mt CO2/yr)")) #emissions from the terrestrial biosphere
  
  #CO2 cumulative lowpass=1
  total <- emisCO2(gdx,level = "regglo",unit="gas",lowpass = 1,cc = TRUE,cumulative = TRUE)/1000
  lu <- emisCO2(gdx,level = "regglo",unit="gas",lowpass = 1,cc = FALSE,cumulative = TRUE)/1000
  cc <- total - lu
  x <- mbind(x,setNames(total,"Emissions|CO2|Land|Cumulative (Gt CO2)"))
  x <- mbind(x,setNames(lu,"Emissions|CO2|Land|Cumulative|+|Land-use Change (Gt CO2)")) #includes land-use change and regrowth of vegetation
  x <- mbind(x,setNames(cc,"Emissions|CO2|Land|Cumulative|+|Climate Change (Gt CO2)")) #emissions from the terrestrial biosphere
  
  #N2O, NOx, NH3
  total <- Emissions(gdx,level="regglo",type=c("n2o_n","nh3_n","no2_n","no3_n"),unit="gas",subcategories=TRUE)
  for (emi in getNames(total,dim=2)){
    prefix<-paste0("Emissions|",reportingnames(emi),"|Land")
    a<-total[,,emi]
    x <- mbind(x,setNames(dimSums(a,dim=3),
                          paste0(prefix,"|+|Agriculture (Mt N2O/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,"awms"],dim=3),
                          paste0(prefix,"|Agriculture|+|Animal Waste Management (Mt N2O/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("inorg_fert","man_crop","resid","SOM","rice","man_past")],dim=3),
                          paste0(prefix,"|Agriculture|+|Agricultural Soils (Mt N2O/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("inorg_fert","rice")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Inorganic Fertilizers (Mt N2O/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("man_crop")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Manure applied to Croplands (Mt N2O/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("resid")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Decay of Crop Residues (Mt N2O/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("SOM")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss (Mt N2O/yr)")))
    #  x <- mbind(x,setNames(dimSums(a[,,c("rice")],dim=3),
    #                     paste0(prefix,"|Agriculture|Agricultural Soils|+|Lower N2O emissions of rice (Mt N2O/yr)")))
    x <- mbind(x,setNames(dimSums(a[,,c("man_past")],dim=3),
                          paste0(prefix,"|Agriculture|Agricultural Soils|+|Pasture (Mt N2O/yr)")))
  }

  #CH4
  a <- collapseNames(Emissions(gdx,level="regglo",type="ch4",unit="gas",subcategories=TRUE),collapsedim = 2)
  x <- mbind(x,setNames(dimSums(a,dim=3),"Emissions|CH4|Land|+|Agriculture (Mt CH4/yr)"))
  x <- mbind(x,setNames(dimSums(a[,,c("rice")],dim=3),"Emissions|CH4|Land|Agriculture|+|Rice (Mt CH4/yr)"))
  x <- mbind(x,setNames(dimSums(a[,,c("awms")],dim=3),"Emissions|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr)"))
  x <- mbind(x,setNames(dimSums(a[,,c("ent_ferm")],dim=3),"Emissions|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr)"))
  
  return(x)
}

