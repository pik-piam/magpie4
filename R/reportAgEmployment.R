#' @title reportAgEmployment
#' @description reports employment in crop+livestock production from MAgPIE results
#' @param gdx GDX file
#' @param type "absolute" for total number of people employed, "share" for share out of working age population
#' @return agricultural employment as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportAgEmployment(gdx)
#'   }
#'

reportAgEmployment <- function(gdx, type = "absolute") {

  out <- agEmployment(gdx, type = type, level = "regglo")

  if (!is.null(out)) {
    if (type == "absolute") {
      out <- setNames(out, "Agricultural employment (mio people)")
    } else if (type == "share") {
      out <- setNames(out, "Share of working age population employed in agriculture (%)")
    } else {
      stop("Type not supported")
    }
  }

  return(out)
}
