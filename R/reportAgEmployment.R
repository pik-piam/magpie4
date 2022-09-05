#' @title reportAgEmployment
#' @description reports employment in crop+livestock production from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param type "absolute" for total number of people employed, "share" for share out of working age population
#' @param detail if TRUE, employment is disaggregated to crop and livestock production, if FALSE only aggregated
#' employment is reported
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @return agricultural employment as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportAgEmployment(gdx)
#'   }
#'

reportAgEmployment <- function(gdx, type = "absolute", detail = FALSE, level = "regglo", dir = ".") {

  out <- agEmployment(gdx, type = type, detail = detail, level = level, dir = dir)

  if (isTRUE(detail)) {
    getNames(out) <- reportingnames(getNames(out))
    out <- mbind(summationhelper(out), setNames(dimSums(out, dim = 3), "Crop and livestock products"))
  } else {
    getNames(out) <- "Crop and livestock products"
  }

  if (!is.null(out)) {
    if (type == "absolute") {
      out <- setNames(out, paste0("Agricultural employment|", getNames(out), " (mio people)"))
    } else if (type == "share") {
      out <- setNames(out, paste0("Share of working age population employed in agriculture|", getNames(out), " (%)"))
    } else {
      stop("Type not supported")
    }
  }

  return(out)
}
