#' @title reportSDG1
#' @description reports all SDG indicators relevant for SDG1 - Poverty
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportSDG3(gdx)
#'   }
#' 

reportSDG1 <- function(gdx) {
  x <- NULL
  
  indicatorname="SDG|SDG01|Per-capita income"
  unit="USD05/cap/yr"
  out <- income(gdx,level="regglo",per_capita = TRUE,after_shock = TRUE)
  getNames(out) <- paste0(indicatorname, " (",unit,")")
  x <- mbind(x,out)
  
  
  #x <- x[,,sort(getNames(x))]  
  return(x)
}

