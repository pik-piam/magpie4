#' @title reportPriceGHG
#' @description reports GHG emission prices
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return GHG emission prices as MAgPIE object
#' @author Florian Humpenoeder, Amsalu W. Yalew
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceGHG(gdx)
#'   }
#' 

reportPriceGHG <- function(gdx) {
  
  #read in data
  t <- readGDX(gdx,"t")
  a <- PriceGHG(gdx,level = "regglo")[,t,]
  a<-a[,,c("co2_c","n2o_n_direct","ch4")]
  getNames(a) <- c("Prices|GHG Emission|CO2 (US$2005/tCO2)",
                     "Prices|GHG Emission|N2O (US$2005/tN2O)",
                     "Prices|GHG Emission|CH4 (US$2005/tCH4)")

  return(a)
}

