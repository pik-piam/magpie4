#' @title reportLocalDemandShares
#' @description reports local demand and production shares based on local consumption
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @param type "prod" or "dem". the former indicates the share of production consumed in its own cluster
#' while the latter indicates how much cluster-level demand is satisfied by local consumption
#' @return  share of food demand at disaggregated level coming from local production as MAgPIE object
#' @author David M Chen
#' @importFrom magpiesets reportingnames reporthelper
#' @importFrom tools toTitleCase
#' @examples
#' \dontrun{
#' x <- reportLocalDemandShares(gdx)
#' }
#'
reportLocalDemandShares <- function(gdx, type = "prod", level = "regglo") {

  if(!is.null(readGDX(gdx,"i40_dem_food_cell", react = "silent"))){

  if (type == "prod") { 
  out <- localDemandShares(gdx, type = type, product_aggr = FALSE, level = level)
  out2 <-  localDemandShares(gdx, type = type, product_aggr = TRUE, level = level)
  getNames(out2) <- "kcr"

  out <- mbind(out, out2)
  repnames <- reportingnames(getNames(out))
  getItems(out, dim = 3.1) <- repnames
  out <- mbind(out, setNames(out[,,"Crop products"], "Primary Crop and Livestock Products"))
  out <- out[, , "Crop products", invert = TRUE]
  getNames(out) <- paste0("Share of Production Satisfying Local Demand|", getNames(out), " (0 - 1)")

  } else if (type == "dem") {
  out1 <- localDemandShares(gdx, type = type, product_aggr = FALSE, level = level)
  out3 <- localDemandShares(gdx, type = type, product_aggr = FALSE, urb_aggr = FALSE, level = level)
  out5 <- localDemandShares(gdx, type = type, product_aggr = FALSE, fvc_aggr = FALSE, level = level)

  out2 <-  localDemandShares(gdx, type = type, product_aggr = TRUE, level = level)
  out2 <- add_dimension(out2, dim = 3.1, add = "k", nm = "k")

  out4 <- localDemandShares(gdx, type = type, product_aggr = TRUE,  urb_aggr = FALSE, level = level)
  out4 <- add_dimension(out4, dim = 3.1, add = "k", nm = "k")

  out6 <- localDemandShares(gdx, type = type, product_aggr = TRUE,  fvc_aggr = FALSE, level = level)
  out6 <- add_dimension(out6, dim = 3.1, add = "k", nm = "kall")
  
  .report <- function(x) {
    repnames <- reportingnames(getNames(x, dim = 1))
    getItems(x, dim = 3.1) <- repnames
  getNames(x) <- paste0("Share of Local Demand Satisfied by Local Production|", getNames(x), " (0 - 1)")
  getNames(x) <- paste(gsub("\\.", "|", getNames(x)), sep = " ")
return(x)
  }

  outL <- list(out1, out2, out3, out4, out5, out6)
  out <- lapply(outL, .report)
  out <- mbind(out)


  }


    return(out)
  
  } else { 
    
 return(NULL)
  }
}
