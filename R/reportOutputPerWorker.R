#' @title reportOutputPerWorker
#' @description reports output per worker in crop+livestock production from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @return output per worker as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportOutputPerWorker(gdx)
#'   }
#'

reportOutputPerWorker <- function(gdx) {

  out <- outputPerWorker(gdx, level = "regglo")

  if (!is.null(out)) {
    out <- setNames(out, "Labor|Productivity|Monetary output per worker (US$05/worker)")
  }

  return(out)
}
