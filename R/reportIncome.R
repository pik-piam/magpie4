#' @title reportIncome
#' @description reports income
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Annual per capita and total income as MAgPIE object (US$2005 MER/cap/yr and million US$05 PPP/yr)
#' @author Florian Humpenoeder, Isabelle Weindl
#' @examples
#' 
#'   \dontrun{
#'     x <- reportIncome(gdx)
#'   }
#' 

reportIncome <- function(gdx) {
  
  #read in regional data
  per_capita <- income(gdx, level = "regglo")
  total <- income(gdx, level = "regglo", per_capita=FALSE)
  #rename
  getNames(per_capita) <- "Income (US$05 PPP/cap/yr)"
  getNames(total) <- "Total income (million US$05 PPP/yr)"
  
  out <- mbind(per_capita,total)

  return(out)
}

