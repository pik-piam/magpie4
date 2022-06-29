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
reportValueMaterialDemand <- function(gdx) {

    out <-  ValueMaterialDemand(gdx,level="regglo")
    out <- dimSums(out, dim = 3.2)
    
    getNames(out,dim=1) <- paste0("Value|Bioeconomy Demand|", 
                                  reportingnames(getNames(out,dim=1)))

   out <-  summationhelper(out)

  return(out)
}
