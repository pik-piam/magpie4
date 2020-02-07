#' @title reportProducerPriceIndex
#' @description reports producer price index
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Producer price index as MAgPIE object Unit: see names
#' @author Isabelle Weindl
#' @examples
#' 
#'   \dontrun{
#'     x <- reportProducerPriceIndex(gdx)
#'   }
#' 

reportProducerPriceIndex <- function(gdx){
  
  #read in data
  primary <- priceIndex(gdx,level="regglo", products="k", baseyear = "y2010", type="producer")
  crops <- priceIndex(gdx,level="regglo", products="kcr", baseyear = "y2010", type="producer")
  livestock <- priceIndex(gdx,level="regglo", products="kli", baseyear = "y2010", type="producer")
  bioenergy <- priceIndex(gdx,level="regglo", products=c("begr","betr"), baseyear = "y2010", type="producer")
  
  #rename
  getNames(primary) <- "Prices|Producer Price Index|Primary agricultural products (Index 2010=100)"
  getNames(crops) <- paste0("Prices|Producer Price Index|",reportingnames("kcr")," (Index 2010=100)",sep="")
  getNames(livestock) <- paste0("Prices|Producer Price Index|",reportingnames("kli")," (Index 2010=100)",sep="")
  getNames(bioenergy) <- "Prices|Producer Price Index|Bioenergy (Index 2010=100)"
  
  out <- mbind(primary,crops,livestock,bioenergy)
  
  return(out)
}