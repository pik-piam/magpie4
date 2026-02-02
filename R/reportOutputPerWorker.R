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
#'
#' @section Output per worker variables:
#' Name | Unit | Meta
#' ---|---|---
#' Labor\|Productivity\|Monetary output per worker | US$2017/worker | Monetary output per agricultural worker
#' @md


reportOutputPerWorker <- function(gdx, level = "regglo") {

  out <- outputPerWorker(gdx, level = level)

  if (!is.null(out)) {
    out <- setNames(out, "Labor|Productivity|Monetary output per worker (US$2017/worker)")
  }

  return(out)
}
