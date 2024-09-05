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
#' @importFrom magclass getNames
#' @importFrom magpiesets reporthelper

reportAgGDP <- function(gdx) {
  # Value added Agricultural GDP
  x <- AgGDP(gdx, level = "regglo")
  getNames(x) <- "Value|Agriculture GDP (million US$2017/yr)"
  return(x)
}
