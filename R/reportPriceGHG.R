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
#'
#' @section GHG price variables:
#' Name | Unit | Meta
#' ---|---|---
#' Prices\|GHG Emission\|CO2 | US$2017/tCO2 | Carbon dioxide emission price
#' Prices\|GHG Emission\|N2O | US$2017/tN2O | Nitrous oxide emission price
#' Prices\|GHG Emission\|CH4 | US$2017/tCH4 | Methane emission price
#' @md

#' @importFrom magpiesets reportingnames

reportPriceGHG <- function(gdx, level = "regglo") {

  #read in data
  t <- readGDX(gdx, "t")
  a <- PriceGHG(gdx, level = level, aggr = "weight")[, t, ]

  if ("emis_source" %in% unlist(strsplit(names(dimnames(a))[[3]], "\\."))) {
    co2 <- a[, , "co2_c"]
    if (dim(co2)[3] > 1) {
      set <- readGDX(gdx, "emis_oneoff", react = "silent")
      if (!is.null(set)) {
        co2 <- co2[, , c(set, "peatland")]
      }
    }
    getNames(co2) <- paste0("Prices|GHG Emission|CO2|",
                            reportingnames(getNames(co2, dim = 2)),
                            " (US$2017/tCO2)")

    n2o <- a[, , "n2o_n_direct"]
    if (dim(n2o)[3] > 1) {
      set <- readGDX(gdx, "emis_source_n51", react = "silent")
      if (!is.null(set)) {
        n2o <- n2o[, , c(set, "peatland")]
      }
    }
    getNames(n2o) <- paste0("Prices|GHG Emission|N2O|",
                            reportingnames(getNames(n2o, dim = 2)),
                            " (US$2017/tN2O)")

    ch4 <- a[, , "ch4"]
    if (dim(ch4)[3] > 1) {
      set <- readGDX(gdx, "emis_source_methane53", react = "silent")
      if (!is.null(set)) {
        ch4 <- ch4[, , c(set, "peatland")]
      }
    }
    getNames(ch4) <- paste0("Prices|GHG Emission|CH4|",
                            reportingnames(getNames(ch4, dim = 2)),
                            " (US$2017/tCH4)")

    a <- mbind(co2, n2o, ch4)
  } else {
    a <- a[, , c("co2_c", "n2o_n_direct", "ch4")]
    getNames(a) <- c(
      "Prices|GHG Emission|CO2 (US$2017/tCO2)",
      "Prices|GHG Emission|N2O (US$2017/tN2O)",
      "Prices|GHG Emission|CH4 (US$2017/tCH4)"
    )
  }

  return(a)
}
