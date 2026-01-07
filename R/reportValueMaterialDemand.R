#' @title reportValueMaterialDemand
#' @description reports value of material demand
#'
#' @export
#'
#' @param gdx GDX file
#' @return magpie object
#' @author David Chen
#' @examples
#' \dontrun{
#' x <- reportValueMaterialDemand(gdx)
#' }
#'
#'
#' @section Material demand value variables:
#' Name | Unit | Meta
#' ---|---|---
#' Value\|Bioeconomy Demand | million US$2017/yr | Total value of bioeconomy material demand
#' Value\|Bioeconomy Demand\|+\|Crops | million US$2017/yr | Value of crop products for material demand
#' Value\|Bioeconomy Demand\|+\|Residues | million US$2017/yr | Value of residues for material demand
#' @md

#' @importFrom magpiesets findset

reportValueMaterialDemand <- function(gdx) {

    out <-  ValueMaterialDemand(gdx, level = "regglo")
    out <- dimSums(out, dim = 3.2)

   getNames(out, dim = 1) <- paste0("Value|Bioeconomy Demand|+|",
                                  reportingnames(getNames(out, dim = 1)))

   total <- dimSums(out, dim = 3)
   getNames(total) <-  "Value|Bioeconomy Demand"
  out <- mbind(total, out)
   getNames(out) <- paste(getNames(out), "(million US$2017/yr)", sep = " ")

  return(out)
}
