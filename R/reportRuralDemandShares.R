#' @title reportRuralDemandShares
#' @description reports rural demand and production shares based on local consumption
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @param type Type of ratio that should be calculated
#' \itemize{
#'        \item \code{all}: How much rural & trad demand as a share of all demand is satisfied locally
#'        \item \code{tradOnly}: How much rural & trad demand as a share of rural & trad demand is satisfied locally
#'        \item \code{potential}: How much total gridded demand is potentially
#'                                satisfied by gridded production
#'        }
#' @return  share of food demand at disaggregated level coming from local production as MAgPIE object
#' @author David M Chen
#' @importFrom magpiesets reportingnames reporthelper
#' @importFrom tools toTitleCase
#' @examples
#' \dontrun{
#' x <- reportruralDemandShares(gdx)
#' }
#'
#' @section Rural demand share variables:
#' Name | Unit | Meta
#' ---|---|---
#' Share of Rural Demand Satisfied by Rural Production\|Primary Crop and Livestock Products | 0 - 1 | Share of rural demand met by local production
#' Share of Total Demand Satisfied by Rural Production\|Primary Crop and Livestock Products | 0 - 1 | Share of total demand met by rural production
#' Share of Total Demand Potentially Satisfied by Local Production | 0 - 1 | Potential local production share
#' @md

#'
reportRuralDemandShares <- function(gdx, type = "tradOnly", level = "regglo") {

if (type == "tradOnly") {
  out1 <- ruralDemandShares(gdx, type = type, product_aggr = FALSE, level = level)
  if(!is.null(out1)){

  out2 <-  ruralDemandShares(gdx, type = type, product_aggr = TRUE, level = level)
  out2 <- add_dimension(out2, dim = 3.1, add = "k", nm = "kcr")

  } else ( return(out1) )
  
  .report <- function(x) {
   repnames <- reportingnames(getNames(x, dim = 1))
   getItems(x, dim = 3.1) <- repnames

    if ("Crop products" %in% repnames){
      getNames(x, dim = 1) <-  "Primary Crop and Livestock Products"
     } 
   getNames(x) <- paste0("Share of Rural Demand Satisfied by Rural Production|", getNames(x), " (0 - 1)")
   getNames(x) <- paste(gsub("\\.", "|", getNames(x)), sep = " ")
 return(x)
  }

  outL <- list(out1, out2)
  out <- lapply(outL, .report)
  out <- mbind(out)

} else if (type == "all") {
  out1 <- ruralDemandShares(gdx, type = type, product_aggr = FALSE, level = level)
  if(!is.null(out1)){

  out2 <-  ruralDemandShares(gdx, type = type, product_aggr = TRUE, level = level)
  out2 <- add_dimension(out2, dim = 3.1, add = "k", nm = "kcr")

  } else ( return(out1) )
  
  .report <- function(x) {
   repnames <- reportingnames(getNames(x, dim = 1))
   getItems(x, dim = 3.1) <- repnames

    if ("Crop products" %in% repnames){
      getNames(x, dim = 1) <-  "Primary Crop and Livestock Products"
     } 
   getNames(x) <- paste0("Share of Total Demand Satisfied by Rural Production|", getNames(x), " (0 - 1)")
   getNames(x) <- paste(gsub("\\.", "|", getNames(x)), sep = " ")
return(x)
  }

  outL <- list(out1, out2)
  out <- lapply(outL, .report)
  out <- mbind(out)

  } else if (type == "potential") {

  out1 <- ruralDemandShares(gdx, type = type, product_aggr = FALSE, level = level)
  out2 <-  ruralDemandShares(gdx, type = type, product_aggr = TRUE, level = level)
  getNames(out2) <- "kcr"

  out <- mbind(out1, out2)
  repnames <- reportingnames(getNames(out))
  getItems(out, dim = 3.1) <- repnames
  out <- mbind(out, setNames(out[,,"Crop products"], "Primary Crop and Livestock Products"))
  out <- out[, , "Crop products", invert = TRUE]
  getNames(out) <- paste0("Share of Total Demand Potentially Satsified by Local Production|", getNames(out), " (0 - 1)")

  }

    return(out)
  
}
