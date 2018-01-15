#' @title reportPriceAgriculture
#' @description reports food commodity prices
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return agricultural commodity prices as MAgPIE object (US\$)
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
  out <- setNames(out, paste0("Prices|Agriculture|",reportingnames(getNames(out))," (US$05/tDM)"))
  
  return(out)
}