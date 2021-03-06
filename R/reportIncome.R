#' @title reportIncome
#' @description reports income
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param type ppp for purchase power parity, mer for market exchange rate
#' @return Annual per capita and total income as MAgPIE object (US$2005 MER/cap/yr and million US$05 PPP/yr)
#' @author Florian Humpenoeder, Isabelle Weindl, Felicitas Beier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportIncome(gdx)
#'   }
#' 

reportIncome <- function(gdx, type="ppp") {
  
  #read in regional data
  per_capita <- income(gdx, type=type, level = "regglo")
  total      <- income(gdx, type=type, level = "regglo", per_capita=FALSE)
  
  #rename
  if (type=="ppp") {
    getNames(per_capita) <- "Income (US$05 PPP/cap/yr)"
    getNames(total)      <- "Total income (million US$05 PPP/yr)"
  } else if (type=="mer") {
    getNames(per_capita) <- "Income (US$05 MER/cap/yr)"
    getNames(total)      <- "Total income (million US$05 MER/yr)"
  } else {
    stop("Please specify reporting type for income units: mer or ppp")
  }

  out <- mbind(per_capita,total)

  return(out)
}

