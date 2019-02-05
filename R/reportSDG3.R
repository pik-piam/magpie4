#' @title reportSDG3
#' @description reports all SDG indicators relevant for SDG3 - Health
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportSDG3(gdx)
#'   }
#' 

reportSDG3 <- function(gdx) {
  x <- NULL
  
  indicatorname="SDG|SDG3|Prevalence of overweight"
  unit="million"
  out <- bodyweight(gdx,level="regglo")
  out <- out[,,"overweight"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG3|Prevalence of obesity"
  unit="million"
  out <- bodyweight(gdx,level="regglo")
  out <- out[,,"obese"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG3|Prevalence of overweight|Children"
  unit="million"
  out <- bodyweight(gdx,level="regglo",age = "underaged")
  out <- out[,,"overweight"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG3|Prevalence of obesity|Children"
  unit="million"
  out <- bodyweight(gdx,level="regglo",age = "underaged")
  out <- out[,,"obese"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG3|Consumption of alcohol"	
  unit="kcal/cap/day"
  out <- Kcal(gdx,level="regglo",products = "alcohol")
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  #x <- x[,,sort(getNames(x))]  
  return(x)
}

