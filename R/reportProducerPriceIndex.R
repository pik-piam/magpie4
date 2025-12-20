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
#'
#' @section Producer price index variables:
#' Name | Unit | Meta
#' ---|---|---
#' Prices\|Index2020\|Agriculture\|Producer\|Primary food products | Index 2020=100 | Producer price index for primary food products
#' Prices\|Index2020\|Agriculture\|Producer\|Crops | Index 2020=100 | Producer price index for crops
#' Prices\|Index2020\|Agriculture\|Producer\|Livestock products | Index 2020=100 | Producer price index for livestock products
#' Prices\|Index2020\|Agriculture\|Producer\|Bioenergy | Index 2020=100 | Producer price index for bioenergy crops
#' Prices\|Index2020\|Agriculture\|Producer\|All agricultural products | Index 2020=100 | Producer price index for all agricultural products
#' @md


reportProducerPriceIndex <- function(gdx, prod_groups = FALSE){

  if (prod_groups) {
  #read in data
  primary <- priceIndex(gdx,level="regglo", products="kfo", baseyear = "y2020", type="producer")
  crops <- priceIndex(gdx,level="regglo", products="kcr", baseyear = "y2020", type="producer")
  livestock <- priceIndex(gdx,level="regglo", products="kli", baseyear = "y2020", type="producer")
  bioenergy <- priceIndex(gdx,level="regglo", products=c("begr","betr"), baseyear = "y2020", type="producer")

  #rename
  getNames(primary) <- "Prices|Index2020|Agriculture|Producer|Primary food products (Index 2020=100)"
  getNames(crops) <- paste0("Prices|Index2020|Agriculture|Producer|",reportingnames("kcr")," (Index 2020=100)",sep="")
  getNames(livestock) <- paste0("Prices|Index2020|Agriculture|Producer|",reportingnames("kli")," (Index 2020=100)",sep="")
  getNames(bioenergy) <- "Prices|Index2020|Agriculture|Producer|Bioenergy (Index 2020=100)"

  out <- mbind(primary,crops,livestock,bioenergy)

  } else {
  pr <- priceIndex(gdx, level="regglo", products="kall", product_aggr = FALSE,
                   baseyear = "y2020", type="producer")

    getNames(pr, dim = 1) <- paste0("Prices|Index2020|Agriculture|Producer|", reportingnames(getNames(pr, dim = 1)),
                                    " (Index 2020=100)")
  crops <- priceIndex(gdx,level="regglo", products="kcr", baseyear = "y2020", type="producer")
  getNames(crops) <- paste0("Prices|Index2020|Agriculture|Producer|",reportingnames("kcr")," (Index 2020=100)",sep="")
  livestock <- priceIndex(gdx,level="regglo", products="kli", baseyear = "y2020", type="producer")
  getNames(livestock) <- paste0("Prices|Index2020|Agriculture|Producer|",reportingnames("kli")," (Index 2020=100)",sep="")
  all <-  priceIndex(gdx,level="regglo", products="kall", baseyear = "y2020", type="producer")
  getNames(all) <- paste0("Prices|Index2020|Agriculture|Producer|",reportingnames("kall")," (Index 2020=100)",sep="")

out <- mbind(pr, crops)
out <- mbind(out, livestock)
out <- mbind(out, all)
  }

  return(out)
}
