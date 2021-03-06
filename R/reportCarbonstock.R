#' @title reportCarbonstock
#' @description Reports the carbon stocks for future MAgPIE projections
#' 
#' @export
#' 
#' @param gdx GDX file
#' @author Kristine Karstens
#' @examples
#'   \dontrun{
#'     x <- reportSOM(gdx)
#'   }

reportCarbonstock <- function(gdx){
  
  x <- carbonstock(gdx, level="regglo", sum_cpool=FALSE, sum_land=TRUE)
  
  out <- mbind(
    setNames(dimSums(x, dim=3), "Resources|Carbon (Mt C)"),
    setNames(x[,,"soilc"],      "Resources|Carbon|+|Soil (Mt C)"),                    
    setNames(x[,,"litc"],       "Resources|Carbon|+|Litter (Mt C)"),   
    setNames(x[,,"vegc"],       "Resources|Carbon|+|Vegetation (Mt C)")
  )
  
  return(out)
  
  
  
}