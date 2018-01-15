#' @title reportPopulation
#' @description reports Population
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return population as MAgPIE object
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPopulation(gdx)
#'   }
#' 

reportPopulation <- function(gdx) {
  
  #read in data
  a <- population(gdx,level = "regglo")
  #rename
  getNames(a) <- "Population (million people)"

  return(a)
}

