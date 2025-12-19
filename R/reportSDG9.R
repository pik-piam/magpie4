#' @title reportSDG9
#' @description reports all SDG indicators relevant for SD9 - Industrial innovation and infrastructure
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportSDG9(gdx)
#'   }
#' 
#'
#' @section SDG9 variables:
#' Name | Unit | Meta
#' ---|---|---
#' SDG\|SDG9\|Manufacturing value added | percentage | Manufacturing value added (placeholder)
#' SDG\|SDG9\|CO2 industry intensity | ton/2005USD | CO2 intensity of industry (placeholder)
#' SDG\|SDG9\|Investment in AgR&D | USD05 | Agricultural R&D investment (placeholder)
#' @md


reportSDG9 <- function(gdx) {
  x <- NULL
  
  indicatorname="SDG|SDG9|Manufacturing value added"	
  unit="percentage"
  #missing

  indicatorname="SDG|SDG9|CO2 industry intensity"	
  unit="ton/2005USD"
  #missing
  
  indicatorname="SDG|SDG9|Investment in AgR&D"	
  unit="USD05"
  #missing
  #out <-
  #getNames(out) <- paste0(indicatorname, " (",unit,")")
  #x <- mbind(x,out)
      
  #x <- x[,,sort(getNames(x))]  
  return(x)
}



