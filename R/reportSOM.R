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
#' @section Actual soil carbon stock variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm | Mt C | Total soil organic carbon stock in top 30 cm
#' Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm\|+\|Cropland Soils | Mt C | SOC stock in cropland soils
#' Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm\|+\|Noncropland Soils | Mt C | SOC stock in non-cropland soils
#'
#' @section Soil carbon stock change variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Actual\|Stock Change\|SOC in top 30 cm | Mt C wrt baseyear | Change in total SOC stock relative to base year
#' Resources\|Soil Carbon\|Actual\|Stock Change\|SOC in top 30 cm\|+\|Cropland Soils | Mt C wrt baseyear | Change in cropland SOC stock relative to base year
#' Resources\|Soil Carbon\|Actual\|Stock Change\|SOC in top 30 cm\|+\|Noncropland Soils | Mt C wrt baseyear | Change in non-cropland SOC stock relative to base year
#'
#' @section Actual soil carbon density variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm | tC/ha | Average SOC density in top 30 cm
#' Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm\|Cropland Soils | tC/ha | SOC density in cropland soils
#' Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm\|Noncropland Soils | tC/ha | SOC density in non-cropland soils
#'
#' @section Target soil carbon stock variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Target\|Stock\|SOC in top 30 cm | Mt C | Target total SOC stock at steady state
#' Resources\|Soil Carbon\|Target\|Stock\|SOC in top 30 cm\|+\|Cropland Soils | Mt C | Target SOC stock in cropland soils
#' Resources\|Soil Carbon\|Target\|Stock\|SOC in top 30 cm\|+\|Noncropland Soils | Mt C | Target SOC stock in non-cropland soils
#'
#' @section Target soil carbon density variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Target\|Density\|SOC in top 30 cm | tC/ha | Target average SOC density at steady state
#' Resources\|Soil Carbon\|Target\|Density\|SOC in top 30 cm\|Cropland Soils | tC/ha | Target SOC density in cropland soils
#' Resources\|Soil Carbon\|Target\|Density\|SOC in top 30 cm\|Noncropland Soils | tC/ha | Target SOC density in non-cropland soils
#'
#' @section Actual carbon share variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Actual\|Carbon Share\|SOC in top 30 cm | tC/tC | SOC relative to natural state
#' Resources\|Soil Carbon\|Actual\|Carbon Share\|SOC in top 30 cm\|Cropland Soils | tC/tC | Cropland SOC relative to natural state
#' Resources\|Soil Carbon\|Actual\|Carbon Share\|SOC in top 30 cm\|Noncropland Soils | tC/tC | Non-cropland SOC relative to natural state
#'
#' @section Target carbon share variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Target\|Carbon Share\|SOC in top 30 cm | tC/tC | Target SOC share at steady state
#' Resources\|Soil Carbon\|Target\|Carbon Share\|SOC in top 30 cm\|Cropland Soils | tC/tC | Target cropland SOC share at steady state
#' Resources\|Soil Carbon\|Target\|Carbon Share\|SOC in top 30 cm\|Noncropland Soils | tC/tC | Target non-cropland SOC share at steady state
#' @md


reportSOM <- function(gdx, baseyear = 1995, level = "regglo") {

  x <- SOM(gdx, level = level, type = "stock")

  out <- mbind(
    setNames(x[, , "total"],       "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm (Mt C)"),
    setNames(x[, , "cropland"],    "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),
    setNames(x[, , "noncropland"], "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
  )

  x <- x - setYears(x[, baseyear, ], NULL)

  out <- mbind(out,
    setNames(x[, , "total"],       paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm (Mt C wrt ", baseyear, ")")),
    setNames(x[, , "cropland"],    paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm|+|Cropland Soils (Mt C wrt ", baseyear, ")")),
    setNames(x[, , "noncropland"], paste0("Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm|+|Noncropland Soils (Mt C wrt ", baseyear, ")"))
  )

  x <- SOM(gdx, level = level, type = "density")

  out <- mbind(out,
    setNames(x[, , "total"],       "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm (tC/ha)"),
    setNames(x[, , "cropland"],    "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm|Cropland Soils (tC/ha)"),
    setNames(x[, , "noncropland"], "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm|Noncropland Soils (tC/ha)")
  )

  x <- SOM(gdx, level = level, reference = "target", type = "stock")

  out <- mbind(out,
    setNames(x[, , "total"],       "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm (Mt C)"),
    setNames(x[, , "cropland"],    "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Cropland Soils (Mt C)"),
    setNames(x[, , "noncropland"], "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm|+|Noncropland Soils (Mt C)")
  )

  x <- SOM(gdx, level = level, reference = "target", type = "density")

  out <- mbind(out,
    setNames(x[, , "total"],       "Resources|Soil Carbon|Target|Density|SOC in top 30 cm (tC/ha)"),
    setNames(x[, , "cropland"],    "Resources|Soil Carbon|Target|Density|SOC in top 30 cm|Cropland Soils (tC/ha)"),
    setNames(x[, , "noncropland"], "Resources|Soil Carbon|Target|Density|SOC in top 30 cm|Noncropland Soils (tC/ha)")
  )

  x <- cshare(gdx, level = level)
  out <- mbind(out,
    setNames(x[, , "total"],       "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm (tC/tC)"),
    setNames(x[, , "cropland"],    "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm|Cropland Soils (tC/tC)"),
    setNames(x[, , "noncropland"], "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm|Noncropland Soils (tC/tC)")
  )

  x <- cshare(gdx, level = level, reference = "target")

  out <- mbind(out,
    setNames(x[, , "total"],       "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm (tC/tC)"),
    setNames(x[, , "cropland"],    "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm|Cropland Soils (tC/tC)"),
    setNames(x[, , "noncropland"], "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm|Noncropland Soils (tC/tC)")
  )

  return(out)
}