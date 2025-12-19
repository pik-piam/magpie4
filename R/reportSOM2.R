#' @title reportSOM2
#' @description Report soil organic carbon stock size for future MAgPIE projections (new som realization)
#'
#' @export
#'
#' @param gdx GDX file
#' @param baseyear baseyear for calculating carbon stock change
#' @author Kristine Karstens
#' @examples
#'   \dontrun{
#'     x <- reportSOM2(gdx)
#'   }
#'
#'
#' @section Soil organic matter variables (SOM2):
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm | Mt C | Actual soil organic carbon stock
#' Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm\|+\|Cropland Soils | Mt C | SOC in cropland soils
#' Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm\|+\|Noncropland Soils | Mt C | SOC in non-cropland soils
#' Resources\|Soil Carbon\|Target\|Stock\|SOC in top 30 cm | Mt C | Target SOC stock at steady state
#' Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm | tC/ha | SOC density
#' Resources\|Soil Carbon\|Actual\|Carbon Share\|SOC in top 30 cm | tC/tC | SOC relative to natural state
#' @md


reportSOM2 <- function(gdx, baseyear = 1995){

  soilStock <- SOM2(gdx, level = "regglo", type="stock", noncropAggr = TRUE)

  somStock <- collapseNames(soilStock[, , "actualstate"])
  natStock <- collapseNames(soilStock[, , "naturalstate"])
  equStock <- collapseNames(soilStock[, , "steadystate"])

  ### STOCK
  unit          <- "(Mt C)"
  zeroOrderName <- "Resources|Soil Carbon|Actual|Stock|SOC in top 30 cm"
  out <- mbind(setNames(somStock[, , "total"],  paste0(zeroOrderName, " ", unit)),
               setNames(somStock[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
               setNames(somStock[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

  zeroOrderName <- "Resources|Soil Carbon|Target|Stock|SOC in top 30 cm"
  out <- mbind(out,
               setNames(equStock[, , "total"],  paste0(zeroOrderName, " ", unit)),
               setNames(equStock[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
               setNames(equStock[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

  ### STOCK CHANGE
  somChang <- somStock - setYears(somStock[, baseyear, ], NULL)
  unit     <- paste0("Mt C wrt ", baseyear)
  zeroOrderName <- "Resources|Soil Carbon|Actual|Stock Change|SOC in top 30 cm"
  out <- mbind(out,
               setNames(somChang[, , "total"],  paste0(zeroOrderName, " ", unit)),
               setNames(somChang[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
               setNames(somChang[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

  ### SOC DEBT
  somDebt <- natStock - somStock
  unit          <- "(Mt C)"
  zeroOrderName <- "Resources|Soil Carbon|Actual|Debt|SOC in top 30 cm"
  out <- mbind(out,
               setNames(somDebt[, , "total"],  paste0(zeroOrderName, " ", unit)),
               setNames(somDebt[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
               setNames(somDebt[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

  ### DENSITY
  soilDens <- SOM2(gdx, level = "regglo", type="density", noncropAggr = TRUE)

  somDens  <- collapseNames(soilDens[, , "actualstate"])
  equDens  <- collapseNames(soilDens[, , "steadystate"])

  unit          <- "(tC/ha)"
  zeroOrderName <- "Resources|Soil Carbon|Actual|Density|SOC in top 30 cm"
  out <- mbind(out,
               setNames(somDens[, , "total"],  paste0(zeroOrderName, " ", unit)),
               setNames(somDens[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
               setNames(somDens[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

  zeroOrderName <- "Resources|Soil Carbon|Target|Density|SOC in top 30 cm"
  out <- mbind(out,
               setNames(equDens[, , "total"],  paste0(zeroOrderName, " ", unit)),
               setNames(equDens[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
               setNames(equDens[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

  ### C SHARE
  unit          <- "(tC/tC)"
  zeroOrderName <- "Resources|Soil Carbon|Actual|Carbon Share|SOC in top 30 cm"
  somShare <- somStock / natStock
  out <- mbind(out,
               setNames(somShare[, , "total"],  paste0(zeroOrderName, " ", unit)),
               setNames(somShare[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
               setNames(somShare[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

  zeroOrderName <- "Resources|Soil Carbon|Target|Carbon Share|SOC in top 30 cm"
  equSomShare   <- equDens / collapseNames(equDens[, , "natveg"])
  out <- mbind(out,
               setNames(equSomShare[, , "total"],  paste0(zeroOrderName, " ", unit)),
               setNames(equSomShare[, , "crop"],   paste0(zeroOrderName, "|+|Cropland Soils ", unit)),
               setNames(equSomShare[, , "natveg"], paste0(zeroOrderName, "|+|Noncropland Soils ", unit)))

  return(out)
}
