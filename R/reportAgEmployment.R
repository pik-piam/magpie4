#' @title reportAgEmployment
#' @description reports employment in crop+livestock production from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param type "absolute" for total number of people employed, "share" for share out of working age population
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return agricultural employment as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportAgEmployment(gdx)
#'   }
#'

reportAgEmployment <- function(gdx, type = "absolute", level = "regglo") {

  out <- agEmployment(gdx, type = type, level = level)

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
