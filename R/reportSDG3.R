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
#'
#' @section SDG3 Health variables:
#' Name | Unit | Meta
#' ---|---|---
#' SDG\|SDG03\|Prevalence of overweight | million | Population with overweight BMI
#' SDG\|SDG03\|Prevalence of obesity | million | Population with obesity
#' SDG\|SDG03\|Prevalence of overweight\|Children | million | Children under 5 with overweight BMI
#' SDG\|SDG03\|Prevalence of obesity\|Children | million | Children under 5 with obesity
#' SDG\|SDG03\|Consumption of alcohol | kcal/cap/day | Daily per-capita caloric intake from alcohol
#' @md


reportSDG3 <- function(gdx) {
  x <- NULL
  
  population <- population(gdx, level="iso", bmi_groups = TRUE ,sex=TRUE ,age=TRUE)
  bodyweight <- bodyweight(gdx, level="regglo", population=population)
  bodyweight_underaged <- bodyweight(gdx,level="regglo",age = "underaged", population=population)
  
  
  indicatorname="SDG|SDG03|Prevalence of overweight"
  unit="million"
  out <- bodyweight
  out <- out[,,"overweight"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG03|Prevalence of obesity"
  unit="million"
  out <- bodyweight
  out <- out[,,"obese"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG03|Prevalence of overweight|Children"
  unit="million"
  out <- bodyweight_underaged
  out <- out[,,"overweight"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG03|Prevalence of obesity|Children"
  unit="million"
  out <- bodyweight_underaged
  out <- out[,,"obese"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG03|Consumption of alcohol"	
  unit="kcal/cap/day"
  out <- Kcal(gdx,level="regglo",products = "alcohol")
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  #x <- x[,,sort(getNames(x))]  
  return(x)
}

