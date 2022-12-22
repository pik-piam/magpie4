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

  if (type == "absolute") {
    main <- "Agricultural employment"
    unit <- " (mio people)"
  } else if (type == "share") {
    main <- "Share of working age population employed in agriculture"
    unit <- " (%)"
  } else {
    stop("Output type not supported")
  }

  if (!is.null(out)) {
    if (isTRUE(detail)) {
      getNames(out)[getNames(out) != "maccs"] <- reportingnames(getNames(out)[getNames(out) != "maccs"])
      getNames(out)[getNames(out) == "maccs"] <- "MACCS"
      getNames(out) <- paste0(main, "|+|", getNames(out), unit)
      out <- mbind(setNames(dimSums(out, dim = 3), paste0(main, unit)), out)
    } else {
      getNames(out) <- paste0(main, unit)
    }
  }

  return(out)
}
