#' @title reportNetForestChange
#' @description reports Net Forest Change
#'
#' @export
#'
#' @param gdx GDX file
#' @return NetForestChange as magclass object (Mha per year)
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportNetForestChange(gdx)
#' }
#'
reportNetForestChange <- function(gdx) {

  x <- NULL

  a <- NetForestChange(gdx, level = "regglo")
  if (!is.null(a)) {
    x <- mbind(x, setNames(dimSums(a, dim = 3), "Resources|NetForestChange (Mha/yr)"))
  }

  return(x)
}
