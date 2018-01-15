#' @title modelstat
#' @description MAgPIE model stat of all optimizations - main optimization and (if used) presolve optimization.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @return A MAgPIE object containing the modelstat
#' @author Jan Philipp Dietrich
#' @examples
#' 
#'   \dontrun{
#'     x <- modelstat(gdx)
#'   }
#'

modelstat <- function(gdx, file=NULL) {
  x <- readGDX(gdx,"p80_modelstat","o_modelstat", format="first_found")
  if(is.null(x)) {
    warning("Modelstat could not be found in GDX file! NULL is returned!")
    return(NULL)
  }
  getNames(x) <- "main"
  tmp <- readGDX(gdx,"p90_modelstat","p90_modstat",react="silent", format="first_found")
  if(!is.null(tmp)) {
    getNames(tmp) <- "presolve"
    x <- mbind(x,tmp)
  }
  out(x,file)
}