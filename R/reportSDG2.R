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
  
  indicatorname="SDG|SDG2|Prevalence of undernourishment"	
  unit="million"
  #missing
  
  indicatorname="SDG|SDG2|Prevalence of underweight"
  unit="million"
  out <- bodyweight(gdx,level="regglo")
  out <- out[,,"underweight"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG2|Prevalence of underweight|Children"	
  unit="million"
  out <- bodyweight(gdx,level="regglo",age = "underaged")
  out <- out[,,"underweight"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG2|Food availability"	
  unit="kcal/cap/day"
  out <- Kcal(gdx,level="regglo")
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG2|Food expenditure share"	
  unit="income"
  out <- FoodExpenditureShare(gdx,level="regglo")
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG2|Food price index"
  unit="index wrt 2005"
  out <- priceIndex(gdx,level="regglo",baseyear = "y2010",products = "kfo")
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG2|Malnutrition under five"	
  unit="million"
  #missing
  
  indicatorname="SDG|SDG2|Prevalence of obesity|Children"	
  unit="million"
  out <- bodyweight(gdx,level="regglo",age = "underaged")
  out <- out[,,"obese"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  indicatorname="SDG|SDG2|Investment in AgR&D"	
  unit="million USD05/yr"
  out <- costs(gdx,sum = FALSE,level="regglo")[,,"TC"]
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  #x <- x[,,sort(getNames(x))]  
  return(x)
}

