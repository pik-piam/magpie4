#' @title reportProducerPriceIndex
#' @description reports producer price index
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Producer price index as MAgPIE object Unit: see names
#' @author Isabelle Weindl, David M CHen
#' @param prod_groups whether to return only product groups

#' @import magpiesets
#' @examples
#' 
#'   \dontrun{
#'     x <- reportProducerPriceIndex(gdx)
#'   }
#' 

reportProducerPriceIndex <- function(gdx, prod_groups = FALSE){
  
  if (prod_groups) {
  #read in data
  primary <- priceIndex(gdx,level="regglo", products="kfo", baseyear = "y2020", type="producer")
  crops <- priceIndex(gdx,level="regglo", products="kcr", baseyear = "y2020", type="producer")
  livestock <- priceIndex(gdx,level="regglo", products="kli", baseyear = "y2020", type="producer")
  bioenergy <- priceIndex(gdx,level="regglo", products=c("begr","betr"), baseyear = "y2020", type="producer")
  
  #rename
  getNames(primary) <- "Prices|Producer Price Index|Primary food products (Index 2020=100)"
  getNames(crops) <- paste0("Prices|Producer Price Index|",reportingnames("kcr")," (Index 2020=100)",sep="")
  getNames(livestock) <- paste0("Prices|Producer Price Index|",reportingnames("kli")," (Index 2020=100)",sep="")
  getNames(bioenergy) <- "Prices|Producer Price Index|Bioenergy (Index 2020=100)"
  
  out <- mbind(primary,crops,livestock,bioenergy) 
  
  } else {
  pr <- priceIndex(gdx, level="regglo", products="kall", product_aggr = FALSE, 
                   baseyear = "y2020", type="producer")
   
    getNames(pr, dim = 1) <- paste0("Prices|Producer Price Index|", reportingnames(getNames(pr, dim = 1)), 
                                    " (Index 2020=100)")
  crops <- priceIndex(gdx,level="regglo", products="kcr", baseyear = "y2020", type="producer")
  getNames(crops) <- paste0("Prices|Producer Price Index|",reportingnames("kcr")," (Index 2020=100)",sep="")
  livestock <- priceIndex(gdx,level="regglo", products="kli", baseyear = "y2020", type="producer")
  getNames(livestock) <- paste0("Prices|Producer Price Index|",reportingnames("kli")," (Index 2020=100)",sep="")
  all <-  priceIndex(gdx,level="regglo", products="kall", baseyear = "y2020", type="producer")
  getNames(all) <- paste0("Prices|Producer Price Index|",reportingnames("kall")," (Index 2020=100)",sep="")

out <- mbind(pr, crops)
out <- mbind(out, livestock)
out <- mbind(out, all)
  }
  
  return(out)
}