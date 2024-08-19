#' @title reportPriceAgriculture
#' @description reports food commodity prices
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return agricultural commodity prices as MAgPIE object (USD)
#' @author Mishko Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceAgriculture(gdx)
#'   }
#' 
#' @importFrom magpiesets reportingnames

reportPriceAgriculture <- function(gdx){
  out <- prices(gdx, level="regglo", products="kall")
  out <- setNames(out, paste0("Prices|Agriculture|",reportingnames(getNames(out))," (US$17/tDM)"))
  
  return(out)
}