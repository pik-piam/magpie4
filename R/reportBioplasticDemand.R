#' @title reportBioplasticDemand
#' @description reports  demand for bioplastic and demand for substrate for bioplastic production from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param detail only relevant for substrate demand. If TRUE, substrate demand is disaggregated by crop type, if
#' FALSE only the aggregated demand is reported.
#' @param level spatial aggregation to report bioplastic/substrate demand (only "reg" or "regglo")
#' @return bioplastic and bioplastic substrate demand as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportBioplasticDemand(gdx)
#'   }
#'
#'
#' @section Bioplastic demand variables:
#' Name | Unit | Meta
#' ---|---|---
#' Demand for bioplastic | Mt/yr | Total bioplastic demand
#' Demand for bioplastic substrate\|Total | Mt DM/yr | Total substrate demand for bioplastic production
#' @md


reportBioplasticDemand <- function(gdx, detail = TRUE, level = "regglo") {

  bioplastic <- bioplasticDemand(gdx, type = "bioplastic", detail = detail, level = level)
  substrate  <- bioplasticDemand(gdx, type = "substrate", detail = detail, level = level)

  if (!is.null(bioplastic) && (sum(bioplastic) != 0)) {
    if (isTRUE(detail)) {
      substrate <- substrate[, , where(substrate != 0)$true$data]
      getNames(substrate) <- reportingnames(getNames(substrate))
      substrate <- mbind(summationhelper(substrate), setNames(dimSums(substrate, dim = 3), "Total"))
      getNames(substrate) <- paste0("Demand for bioplastic substrate|", getNames(substrate))
    } else {
      getNames(substrate) <- "Demand for bioplastic substrate"
    }

    getNames(substrate) <- paste0(getNames(substrate), " (Mt DM/yr)")
    getNames(bioplastic) <- "Demand for bioplastic (Mt/yr)"

    out <- mbind(bioplastic, substrate)
  } else {
    out <- NULL
  }

  return(out)
}
