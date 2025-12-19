#' @title reportSOM
#' @description Report soil organic carbon stock size for future MAgPIE projections
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param baseyear baseyear for calculating carbon stock change
#' @author Kristine Karstens
#' @examples
#'   \dontrun{
#'     x <- reportSOM(gdx)
#'   }
#' 
#'
#' @section Soil organic carbon stock variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm | Mt C | Total soil organic carbon stock in top 30 cm
#' Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm\|+\|Cropland Soils | Mt C | SOC stock in cropland soils
#' Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm\|+\|Noncropland Soils | Mt C | SOC stock in non-cropland soils
#'
#' @section Soil organic carbon density variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm | tC/ha | Average SOC density in top 30 cm
#' Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm\|Cropland Soils | tC/ha | SOC density in cropland soils
#' Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm\|Noncropland Soils | tC/ha | SOC density in non-cropland soils
#' @md


reportSOM <- function(gdx, baseyear=1995){

  x <- SOM(gdx, level="regglo", type="stock")
  
  out <- mbind(
    setNames(x[,,"total"],       "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
  )

  x <- x - setYears(x[,baseyear,],NULL)
  
  out <- mbind(out,
    setNames(x[,,"total"],       paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm (Mt C wrt ",baseyear,")")),                    
    setNames(x[,,"cropland"],    paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm|+|Cropland Soils (Mt C wrt ",baseyear,")")),   
    setNames(x[,,"noncropland"], paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm|+|Noncropland Soils (Mt C wrt ",baseyear,")"))
  )
  
  x <- SOM(gdx, level="regglo", type="density")
  
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm|Cropland Soils (tC/ha)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm|Noncropland Soils (tC/ha)")
  )
  
  x <- SOM(gdx, level="regglo", reference="target", type="stock")
  
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm (Mt C)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
  )
  
  x <- SOM(gdx, level="regglo", reference="target", type="density")
  
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Target|Density|SOC in top 30 cm (tC/ha)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Target|Density|SOC in top 30 cm|Cropland Soils (tC/ha)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Target|Density|SOC in top 30 cm|Noncropland Soils (tC/ha)")
  ) 
  
  x <- cshare(gdx, level="regglo")
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm (tC/tC)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm|Cropland Soils (tC/tC)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm|Noncropland Soils (tC/tC)")
  )
  
  x <- cshare(gdx, level="regglo", reference="target")
  
  out <- mbind(out,
    setNames(x[,,"total"],       "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm (tC/tC)"),                    
    setNames(x[,,"cropland"],    "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm|Cropland Soils (tC/tC)"),   
    setNames(x[,,"noncropland"], "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm|Noncropland Soils (tC/tC)")
  )
  
  return(out)
}