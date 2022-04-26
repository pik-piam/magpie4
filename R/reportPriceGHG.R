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
  a <- PriceGHG(gdx,level = "regglo",aggr = "weight")[,t,]

  if("emis_source" %in% unlist(strsplit(names(dimnames(a))[[3]],"\\."))) {
    co2 <- a[,,"co2_c"]
    getNames(co2) <- paste0(gsub("co2_c\\.","Prices|GHG Emission|CO2|",getNames(co2))," (US$2005/tCO2)")
    n2o <- a[,,"n2o_n_direct"]
    getNames(n2o) <- paste0(gsub("n2o_n_direct\\.","Prices|GHG Emission|N2O|",getNames(n2o))," (US$2005/tN2O)")
    ch4 <- a[,,"ch4"]
    getNames(ch4) <- paste0(gsub("ch4\\.","Prices|GHG Emission|CH4|",getNames(ch4))," (US$2005/tCH4)")
    a <- mbind(co2,n2o,ch4)
  } else {
    a<-a[,,c("co2_c","n2o_n_direct","ch4")]
    getNames(a) <- c("Prices|GHG Emission|CO2 (US$2005/tCO2)",
                     "Prices|GHG Emission|N2O (US$2005/tN2O)",
                     "Prices|GHG Emission|CH4 (US$2005/tCH4)")
  }


  return(a)
}

