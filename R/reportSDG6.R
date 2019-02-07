#' @title reportSDG6
#' @description reports all SDG indicators relevant for SDG6 - Access to Water
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportSDG6(gdx)
#'   }
#' 

reportSDG6 <- function(gdx) {
  x <- NULL
  
  indicatorname="SDG|SDG6|Safe sanitation"	
  unit="fraction"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Safe wastewater"	
  unit="fraction"
  #missing (retrieve from moinput?)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Water quality"	
  unit="fraction"
  #missing  
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|N water loading"	
  unit="Mt N/yr"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|P water loading"	
  unit="Mt P/yr"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Fertilizer use"	
  unit="Mt N/yr"
  # Def.: Nitrogen fertilizer use (retrieved from NitrogenBudget.R --> UNIT??)
  out <- collapseNames(readGDX(gdx,"ov_nr_inorg_fert_reg",format="first_found",select=list(type="level"))[,,"crop"])
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Nitrate concentration in water"	
  unit="tN/km3"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname="SDG|SDG6|Water use efficiency"	
  unit="rate"
  #missing
  # Def.: a country's total gross domestic product (GDP) divided by total freshwater withdrawals (OurWorldInData: sdg-tracker.org????)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Water stress|Agriculture"	
  unit="fraction"
  # Def.: total quantity of freshwater withdrawals (agriculture, industry, domestic) as a share of available freshwater resources
  # Def. (HERE): freshwater withdrawals [here: for agriculture] (water_usage, km^3/yr) as a proportion of available freshwater (water_avail, km^3)
  out <- water_usage(gdx,level="regglo",users="kcr",sum=TRUE,digits=3)/water_avail(gdx,level="regglo")
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|People under water stress"	
  unit="million"
  #missing (Def.: number of people living in water stressed region??)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Environmental flow exceedance"	
  unit="percentage of land area"
  #missing (given in MAgPIE)
  # Def.: Level of environmental water flow exceedance (of agirculture in growing season)
  # Def. (MAgPIE): water environmental flow violation volume
  #out <- WaterEFVvolume(gdx)
  #getNames(out) <- paste0(indicatorname, " (",unit,")")
  #x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Agricultural water use"	
  unit="km3/yr"
  # Def.: water usage in agriculture 
  out <- water_usage(gdx,level="regglo",users="kcr",sum=TRUE,digits=3)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Water-related ecosystems"	
  unit="million ha"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
    
  #x <- x[,,sort(getNames(x))]  
  return(x)
}




