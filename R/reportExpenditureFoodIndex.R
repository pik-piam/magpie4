#' @title reportExpenditureFoodIndex
#' @description reports food expenditure index and food expenditure index
#'              corrected for emission costs
#'
#' @export
#'
#' @param gdx        GDX file
#' @param baseyear   Baseyear of the price index
#' @param basketyear Year of reference food basket
#'                   (should be in the past for comparison of different runs to
#'                   have identical and comparable food basket)
#'
#' @return Food expenditure index as MAgPIE object
#' @author Felicitas Beier
#' @examples
#' \dontrun{
#' x <- reportPriceFoodIndex(gdx)
#' }
#'
#' @section Food expenditure index variables:
#' Name | Unit | Meta
#' ---|---|---
#' Prices\|Food Expenditure Index | Index 2010=100 | Food expenditure index (all food products)
#' Prices\|Food Expenditure Index\|Plant-based food products | Index 2010=100 | Food expenditure index for plant-based products
#' Prices\|Food Expenditure Index\|Livestock food products | Index 2010=100 | Food expenditure index for livestock products
#' Prices\|Food Expenditure Index corrected for ghg costs | Index 2010=100 | Food expenditure index corrected for GHG costs
#' Prices\|Agricultural Primary Products Expenditure Index | Index 2010=100 | Agricultural primary products expenditure index
#' @md

#'
reportExpenditureFoodIndex <- function(gdx, baseyear = "y2010", basketyear = "y2010") {

  x <- NULL 
  for (valueAdded in c(TRUE, FALSE)) {
  # corrected food expenditure index all food products
  x1 <- expenditureIndexFood(gdx, level = "regglo", products = "kfo",
                             basketyear = basketyear, baseyear = baseyear,
                             ghgtax = TRUE, round = TRUE, valueAdded = valueAdded)

  if (valueAdded) {
    name <- "Prices|Food Expenditure Index"
  } else {
    name <- "Prices|Agricultural Primary Products Expenditure Index"
  }
  getNames(x1) <- paste0(name, " corrected for ghg costs (Index ", gsub("\\y", "", baseyear), "=100)")

  # plant-based food products
  x2 <- expenditureIndexFood(gdx, level = "regglo", products = "kfo_pp",
                             basketyear = basketyear, baseyear = baseyear,
                             ghgtax = TRUE, round = TRUE, valueAdded = valueAdded)
  getNames(x2) <- paste0(name, " corrected for ghg costs|Plant-based food products (Index ", gsub("\\y", "", baseyear), "=100)")

  # livestock food products
  x3 <- expenditureIndexFood(gdx, level = "regglo", products = "kfo_lp",
                             basketyear = basketyear, baseyear = baseyear,
                             ghgtax = TRUE, round = TRUE, valueAdded = valueAdded)
  getNames(x3) <- paste0(name, " corrected for ghg costs|Livestock food products (Index ", gsub("\\y", "", baseyear), "=100)")

  # food expenditure index all food products
  x4 <- expenditureIndexFood(gdx, level = "regglo", products = "kfo",
                             basketyear = basketyear, baseyear = baseyear,
                             ghgtax = FALSE, round = TRUE, valueAdded = valueAdded)
  getNames(x4) <- paste0(name, " (Index ", gsub("\\y", "", baseyear), "=100)")

  # food expenditure index all food products
  x5 <- expenditureIndexFood(gdx, level = "regglo", products = "kfo_pp",
                             basketyear = basketyear, baseyear = baseyear,
                             ghgtax = FALSE, round = TRUE, valueAdded = valueAdded)
  getNames(x5) <- paste0(name, "|Plant-based food products (Index ", gsub("\\y", "", baseyear), "=100)")

  # food expenditure index all food products
  x6 <- expenditureIndexFood(gdx, level = "regglo", products = "kfo_lp",
                             basketyear = basketyear, baseyear = baseyear,
                             ghgtax = FALSE, round = TRUE, valueAdded = valueAdded)
  getNames(x6) <- paste0(name, "|Livestock food products (Index ", gsub("\\y", "", baseyear), "=100)")

  x <- mbind(x, x1, x2, x3, x4, x5, x6)
  }

  return(x)
}
