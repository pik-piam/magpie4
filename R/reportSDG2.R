#' @title reportSDG2
#' @description reports all SDG indicators relevant for SD2 - Hunger
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportSDG2(gdx)
#'   }
#' 

reportSDG2 <- function(gdx) {
  x <- NULL
  
  population <- population(gdx, level="iso", bmi_groups = TRUE, sex=TRUE, age=TRUE)
  bodyweight <- bodyweight(gdx, level="regglo", population=population)
  bodyweight_underaged <- bodyweight(gdx, level="regglo", age = "underaged", population=population)
  
  indicatorname="SDG|SDG02|Prevalence of undernourishment"	
  unit="million"
  #missing
  
  indicatorname="SDG|SDG02|Prevalence of underweight"
  unit="million"
  out <- bodyweight
  out <- out[,,"underweight"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG02|Prevalence of underweight|Children"	
  unit="million"
  out <- bodyweight_underaged
  out <- out[,,"underweight"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG02|Food availability"	
  unit="kcal/cap/day"
  out <- Kcal(gdx,level="regglo")
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG02|Food expenditure share"	
  unit="income"
  out <- FoodExpenditureShare(gdx,level="regglo")
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG02|Agricultural commodity price index wrt 2020"
  unit="1"
  out <- priceIndex(gdx,level="regglo",baseyear = "y2020",products = "kfo")/100
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG02|Malnutrition under five"	
  unit="million"
  #missing
  
  indicatorname="SDG|SDG02|Prevalence of obesity|Children"	
  unit="million"
  out <- bodyweight_underaged
  out <- out[,,"obese"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG02|Investment in AgR&D"	
  unit="million USD05/yr"
  out <- costs(gdx,sum = FALSE,level="regglo")[,,"TC"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  #x <- x[,,sort(getNames(x))]  
  return(x)
}

