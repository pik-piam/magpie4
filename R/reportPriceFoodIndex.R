#' @title reportPriceFoodIndex
#' @description reports food price index
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param baseyear baseyear of the price index

#' @return Food price index as MAgPIE object Unit: see names
#' @author Florian Humpenoeder, Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceFoodIndex(gdx)
#'   }
#' 

reportPriceFoodIndex <- function(gdx, baseyear = "y2020"){
  x <- NULL
  
  # all food products
  x1 <- priceIndex(gdx,level="regglo", products="kfo", baseyear = baseyear)/100
  getNames(x1) <- paste0("Prices|Index",gsub("\\y","",baseyear),"|Agriculture|Food products (1)")
  x <- mbind(x,x1)

  # plant-based food products
  x1 <- priceIndex(gdx,level="regglo", products="kfo_pp", baseyear = baseyear)/100
  getNames(x1) <- paste0("Prices|Index",gsub("\\y","",baseyear),"|Agriculture|Food products|Plant-based (1)")
  x <- mbind(x,x1)
  
  # plant-based food products: Maize/Corn
  x1 <- priceIndex(gdx,level="regglo", products="maiz", baseyear = baseyear)/100
  getNames(x1) <- paste0("Prices|Index",gsub("\\y","",baseyear),"|Agriculture|Food products|Plant-based|Maize (1)")
  x <- mbind(x,x1)
  
  # plant-based food products: Rice
  x1 <- priceIndex(gdx,level="regglo", products="rice_pro", baseyear = baseyear)/100
  getNames(x1) <- paste0("Prices|Index",gsub("\\y","",baseyear),"|Agriculture|Food products|Plant-based|Rice (1)")
  x <- mbind(x,x1)
  
  # plant-based food products: Soybean
  x1 <- priceIndex(gdx,level="regglo", products="soybean", baseyear = baseyear)/100
  getNames(x1) <- paste0("Prices|Index",gsub("\\y","",baseyear),"|Agriculture|Food products|Plant-based|Soybean (1)")
  x <- mbind(x,x1)
  
  # plant-based food products: Temperate cereals / Wheat
  x1 <- priceIndex(gdx,level="regglo", products="tece", baseyear = baseyear)/100
  getNames(x1) <- paste0("Prices|Index",gsub("\\y","",baseyear),"|Agriculture|Food products|Plant-based|Temperate cereals (1)")
  x <- mbind(x,x1)
  
  # livestock food products
  x1 <- priceIndex(gdx,level="regglo", products="kfo_lp", baseyear = baseyear)/100
  getNames(x1) <- paste0("Prices|Index",gsub("\\y","",baseyear),"|Agriculture|Food products|Livestock (1)")
  x <- mbind(x,x1)
  
  return(x)
}
