#' @title reportLocalDemandShare
#' @description reports labor and capital cost share out of factor costs from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @return  share of food demand at disaggregated level coming from local production as MAgPIE object
#' @author David M Chen
#' @importFrom magpiesets reportingnames reporthelper
#' @importFrom tools toTitleCase
#' @examples
#' \dontrun{
#' x <- reportLocalDemand(gdx)
#' }
#'
reportLocalDemandShare <- function(gdx, level = "regglo") {

  out <- localDemandShare(gdx, product_aggr = FALSE, level = level)
  repnames <- reportingnames(getItems(out, dim = 3.1))

  getItems(out, dim = 3.1) <- repnames
  getNames(out) <- paste(gsub("\\.", "|", getNames(out)), "(0 - 1)", sep = " ")
  getNames(out) <- paste0("Share of Local Consumption|", getNames(out))

  #products aggregated
  out1 <- localDemandShare(gdx, product_aggr = TRUE, level = level)

  getNames(out1) <- paste(gsub("\\.", "|", getNames(out1)), "(0 - 1)", sep = " ")
  getNames(out1) <- paste0("Share of Local Consumption|", getNames(out1))

  out <- mbind(out1, out)

  return(out)
}
