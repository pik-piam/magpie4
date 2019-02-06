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
  #missing  (in MAgPIE!)
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Nitrate concentration in water"	
  unit="tN/km3"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)

  indicatorname="SDG|SDG6|Water use efficiency"	
  unit="rate"
  #missing
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Water stress"	
  unit="fraction"
  # freshwater withdrawals as a proportion of available freshwater
  out <- water_avail(gdx,level="regglo")
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
  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x <- mbind(x,out)
  
  indicatorname="SDG|SDG6|Agricultural water use"	
  unit="km3/yr"
  out <- reportWaterUsage(gdx)
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




