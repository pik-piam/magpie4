#' @title reportSDG9
#' @description reports all SDG indicators relevant for SD9 - Industrial innovation and infrastructure
#'
#' @export
#'
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Felicitas Beier
#' @examples
#'
#'   \dontrun{
#'     x <- reportSDG9(gdx)
#'   }
#'


reportSDG9 <- function(gdx) {
  x <- NULL

  indicatorname <- "SDG|SDG9|Manufacturing value added"
  unit <- "percentage"
  #missing

  indicatorname <- "SDG|SDG9|CO2 industry intensity"
  unit <- "ton/2005USD"
  #missing

  indicatorname <- "SDG|SDG9|Investment in AgR&D"
  unit <- "USD05"
  #missing
  #out <-
  #getNames(out) <- paste0(indicatorname, " (",unit,")")
  #x <- mbind(x,out)

  #x <- x[,,sort(getNames(x))]
  return(x)
}
