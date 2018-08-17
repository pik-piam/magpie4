#' @title reportIncome
#' @description reports income
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Annual per capita income as MAgPIE object (US$2005 MER/cap/yr)
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportIncome(gdx)
#'   }
#' 

reportIncome <- function(gdx) {
  
  #read in regional data
  a <- income(gdx,level = "regglo")
  #rename
  getNames(a) <- "Income (US$05 PPP/cap/yr)"

  return(a)
}

