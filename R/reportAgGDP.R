#' @title reportAgGDP
#' @description reports MAgPIE Agricultural GDP Mio. USD05 MER
#'
#' @export
#'
#' @param gdx GDX file
#' @return Magpie object
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' x <- reportAgGDP(gdx)
#' }
#'
#' @section Agricultural GDP variables:
#' Name | Unit | Meta
#' ---|---|---
#' Value\|Agriculture GDP | million US$2017/yr | Agricultural value added (GDP from agriculture sector)
#' @md
#' @importFrom magclass getNames
reportAgGDP <- function(gdx, level = "regglo") {
  # Value added Agricultural GDP
  x <- AgGDP(gdx, level = level)
  getNames(x) <- "Value|Agriculture GDP (million US$2017/yr)"
  return(x)
}
