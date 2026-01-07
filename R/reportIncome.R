#' @title reportIncome
#' @description reports income
#'
#' @export
#'
#' @param gdx  GDX file
#' @param type ppp for purchase power parity, mer for market exchange rate
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return Annual per capita and total income as MAgPIE object (US$2017 MER/cap/yr and million US$17 PPP/yr)
#' @author Florian Humpenoeder, Isabelle Weindl, Felicitas Beier
#' @examples
#' \dontrun{
#' x <- reportIncome(gdx)
#' }
#'
#' @section Income variables:
#' Name | Unit | Meta
#' ---|---|---
#' Income per capita PPP | US$2017 PPP/cap/yr | GDP per capita converted to US$2017 using purchasing power parity (PPP)
#' Income PPP | million US$2017 PPP/yr | GDP converted to US$2017 using purchasing power parity (PPP)
#' Income per capita MER | US$2017 MER/cap/yr | GDP per capita converted to US$2017 at market exchange rate (MER)
#' Income MER | million US$2017 MER/yr | GDP converted to US$2017 at market exchange rate (MER)
#' @md

#'
reportIncome <- function(gdx, type = "ppp", level = "regglo") {

  # read in regional data
  perCapita  <- income(gdx, type = type, level = level)
  total      <- income(gdx, type = type, level = level, per_capita = FALSE)

  # rename
  if (type == "ppp") {

    getNames(perCapita)  <- "Income per capita PPP (US$2017 PPP/cap/yr)"
    getNames(total)      <- "Income PPP (million US$2017 PPP/yr)"

  } else if (type == "mer") {

    getNames(perCapita)  <- "Income per capita MER (US$2017 MER/cap/yr)"
    getNames(total)      <- "Income MER (million US$2017 MER/yr)"

  } else {
    stop("Please specify reporting type for income units: mer or ppp")
  }

  out <- mbind(perCapita, total)

  return(out)
}
