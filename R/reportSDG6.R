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
  
  indicatorname="SDG|SDG06|Safe sanitation"	
  unit="fraction"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|Safe wastewater"	
  unit="fraction"
  #missing (retrieve from moinput?)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|Water quality"	
  unit="fraction"
  #missing  
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|N water loading"	
  unit="Mt N/yr"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # runoff
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|P water loading"	
  unit="Mt P/yr"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|Fertilizer use"	
  unit="Mt N/yr"
  # Def.: Nitrogen fertilizer use
  out <- collapseNames(readGDX(gdx,"ov_nr_inorg_fert_reg",format="first_found",select=list(type="level"))[,,"crop"])
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  out <- mbind(out,setCells(dimSums(out,dim=1),"GLO"))
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|Nitrate concentration in water"	
  unit="tN/km3"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname="SDG|SDG06|Water use efficiency"	
  unit="rate"
  #missing
  # Def.: a country's total gross domestic product (GDP) divided by total freshwater withdrawals (OurWorldInData: sdg-tracker.org????)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|Water stress"	
  unit="fraction"
  # Def.: total quantity of freshwater withdrawals (agriculture, industry, domestic; km^3) as a share of total available freshwater resources (km^3)
  out <- water_usage(gdx,level="regglo",users=c("agriculture", "industry", "electricity", "domestic"),sum=TRUE)/water_avail(gdx,level="regglo",sources=NULL,sum=TRUE)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|People under water stress"	
  unit="million"
  #missing (Def.: number of people living in water stressed region??)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|Environmental flow exceedance"	
  unit="percentage of land area"
  # Def.: Area affected by environmental water flow violation
  out <- water_EFexceedance(gdx,level="regglo")
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  
  #getNames(out) <- paste0(indicatorname, " (",unit,")")
  #x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|Agricultural water use"	
  unit="km3/yr"
  # Def.: water usage in agriculture 
  out <- water_usage(gdx,level="regglo",users="agriculture",sum=TRUE,digits=3)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG06|Water-related ecosystems"	
  unit="million ha"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
    
  #x <- x[,,sort(getNames(x))]  
  return(x)
}




