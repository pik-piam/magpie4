#' @title reportPriceFoodIndex
#' @description reports food price index
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @param baseyear baseyear of the price index

#' @return Food price index as MAgPIE object Unit: see names
#' @author Florian Humpenoeder, Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceFoodIndex(gdx)
#'   }
#' 
#'
#' @section Food price index variables:
#' Name | Unit | Meta
#' ---|---|---
#' Prices\|Index\|Agriculture\|Food products | 1 | Food price index relative to baseyear
#' Prices\|Index\|Agriculture\|Food products\|Plant-based | 1 | Plant-based food price index
#' Prices\|Index\|Agriculture\|Food products\|Plant-based\|Maize | 1 | Maize price index
#' Prices\|Index\|Agriculture\|Food products\|Plant-based\|Rice | 1 | Rice price index
#' Prices\|Index\|Agriculture\|Food products\|Plant-based\|Soybean | 1 | Soybean price index
#' Prices\|Index\|Agriculture\|Food products\|Plant-based\|Temperate cereals | 1 | Wheat/temperate cereals price index
#' Prices\|Index\|Agriculture\|Food products\|Livestock | 1 | Livestock food price index
#' @md


reportPriceFoodIndex <- function(gdx, baseyear = "y2020", level = "regglo") {
  # all food products
  x1 <- priceIndex(gdx, level = level, products = "kfo", baseyear = baseyear) / 100
  getNames(x1) <- paste0("Prices|Index", gsub("\\y", "", baseyear), "|Agriculture|Food products (1)")

  # plant-based food products
  x2 <- priceIndex(gdx, level = level, products = "kfo_pp", baseyear = baseyear) / 100
  getNames(x2) <- paste0("Prices|Index", gsub("\\y", "", baseyear), "|Agriculture|Food products|Plant-based (1)")

  # plant-based food products: Maize/Corn
  x3 <- priceIndex(gdx, level = level, products = "maiz", baseyear = baseyear) / 100
  getNames(x3) <- paste0("Prices|Index", gsub("\\y", "", baseyear), "|Agriculture|Food products|Plant-based|Maize (1)")

  # plant-based food products: Rice
  x4 <- priceIndex(gdx, level = level, products = "rice_pro", baseyear = baseyear) / 100
  getNames(x4) <- paste0("Prices|Index", gsub("\\y", "", baseyear), "|Agriculture|Food products|Plant-based|Rice (1)")

  # plant-based food products: Soybean
  x5 <- priceIndex(gdx, level = level, products = "soybean", baseyear = baseyear) / 100
  getNames(x5) <- paste0("Prices|Index", gsub("\\y", "", baseyear), "|Agriculture|Food products|Plant-based|Soybean (1)")

  # plant-based food products: Temperate cereals / Wheat
  x6 <- priceIndex(gdx, level = level, products = "tece", baseyear = baseyear) / 100
  getNames(x6) <- paste0("Prices|Index", gsub("\\y", "", baseyear), "|Agriculture|Food products|Plant-based|Temperate cereals (1)")

  # livestock food products
  x7 <- priceIndex(gdx, level = level, products = "kfo_lp", baseyear = baseyear) / 100
  getNames(x7) <- paste0("Prices|Index", gsub("\\y", "", baseyear), "|Agriculture|Food products|Livestock (1)")

  x <- mbind(x1, x2, x3, x4, x5, x6, x7)

  return(x)
}
