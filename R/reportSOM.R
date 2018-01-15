#' @title reportSOM
#' @description Report soil organic carbon stock size for future MAgPIE projections
#' 
#' @export
#' 
#' @param gdx GDX file
#' @author Kristine Karstens
#' @examples
#'   \dontrun{
#'     x <- reportSOM(gdx)
#'   }
#' 

reportSOM <- function(gdx){

  x <- SOM(gdx, level="regglo")
  
  out <- mbind(
    setNames(x[,,"total"],       "Resources|Carbon Stocks|Soil Carbon in top 30 cm (Mt C)"),                    
    setNames(x[,,"cropland"],    "Resources|Carbon Stocks|Soil Carbon in top 30 cm|+|Cropland Soils (Mt C)"),   
    setNames(x[,,"noncropland"], "Resources|Carbon Stocks|Soil Carbon in top 30 cm|+|Noncropland Soils (Mt C)")
  )

  return(out)
}