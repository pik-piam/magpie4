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
#' @importFrom magpiesets findset

reportValueMaterialDemand <- function(gdx) {

    out <-  ValueMaterialDemand(gdx, level = "regglo")
    out <- dimSums(out, dim = 3.2)

   getNames(out, dim = 1) <- paste0("Value|Bioeconomy Demand|+|",
                                  reportingnames(getNames(out, dim = 1)))

   total <- dimSums(out, dim = 3)
   getNames(total) <-  "Value|Bioeconomy Demand"
  out <- mbind(total, out)
   getNames(out) <- paste(getNames(out), "(million US$17/yr)", sep = " ")

  return(out)
}
