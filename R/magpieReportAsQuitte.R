
#' @title magpieReportAsQuitte
#' @description Converts a MAgPIE report object to a quitte object.
#'
#' @param report A MAgPIE object containing the report to be converted.
#' @return The report as a quitte object with the global region named "World".
#' @author Patrick Rein
#' @export
magpieReportAsQuitte <- function(report) {
  if (!requireNamespace("quitte", quietly = TRUE)) {
    stop("Package 'quitte' needed to convert to quitte.")
  }

  qu <- quitte::as.quitte(report)

  # as.quitte converts "World" into "GLO". But we want to keep "World" and therefore undo these changes
  qu <- droplevels(qu)
  levels(qu$region)[levels(qu$region) == "GLO"] <- "World"
  qu$region <- factor(qu$region, levels = sort(levels(qu$region)))

  if (all(is.na(qu$value))) {
    stop("No values in reporting!")
  }

  return(qu)
}
