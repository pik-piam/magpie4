#' @title reportPopulation
#' @description reports Population
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return population as MAgPIE object
#' @author Florian Humpenoeder
#' @examples
#'
#'   \dontrun{
#'     x <- reportPopulation(gdx)
#'   }
#'
#'
#' @section Population variables:
#' Name | Unit | Meta
#' ---|---|---
#' Population | million people | Total population
#' @md


reportPopulation <- function(gdx,level = "regglo") {

  #read in data
  a <- population(gdx,level = level)
  #rename
  getNames(a) <- "Population (million people)"

  return(a)
}

