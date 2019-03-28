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

  x <- SOM(gdx, level="regglo", type="stock")
  
  out <- mbind(
    setNames(x[,,"total"],       "Resources|Soil Carbon|Actual|Stocks|SOC in top 30 cm (Mt C)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Actual|Stocks|SOC in top 30 cm|+|Cropland Soils (Mt C)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Actual|Stocks|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
  )

  x <- SOM(gdx, level="regglo", type="density")
  
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm|+|Cropland Soils (tC/ha)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm|+|Noncropland Soils (tC/ha)")
  )
  
  x <- SOM(gdx, level="regglo", reference="target", type="stock")
  
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Target|Stocks|SOC in top 30 cm (Mt C)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Target|Stocks|SOC in top 30 cm|+|Cropland Soils (Mt C)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Target|Stocks|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
  )
  
  x <- SOM(gdx, level="regglo", reference="target", type="density")
  
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Target|Density|SOC in top 30 cm (tC/ha)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Target|Density|SOC in top 30 cm|+|Cropland Soils (tC/ha)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Target|Density|SOC in top 30 cm|+|Noncropland Soils (tC/ha)")
  ) 
  
  x <- cshare(gdx, level="regglo")
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm (tC/tC)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm|+|Cropland Soils (tC/tC)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm|+|Noncropland Soils (tC/tC)")
  )
  
  x <- cshare(gdx, level="regglo", reference="target")
  
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm (tC/tC)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm|+|Cropland Soils (tC/tC)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm|+|Noncropland Soils (tC/tC)")
  )
  return(out)
}