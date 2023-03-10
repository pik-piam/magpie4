#' @title getReportMAgPIE2REMIND
#' @description Based on a MAgPIE gdx file, a report is generated containing
#' only the variables relevant for the coupling with REMIND. Basically a copy
#' of getReport, but calling less 'reportXY()' functions.
#'
#' @export
#'
#' @param gdx      GDX file
#' @param file     A file name the output should be written to using write.report.
#'                 If NULL the report is returned instead as a MAgPIE object.
#' @param scenario Name of the scenario used for the list-structure of a
#'                 reporting object (x$scenario$MAgPIE).
#'                 If NULL the report is returned instead as a MAgPIE object.
#' @return A MAgPIE object containing the report.
#' @details Reports are organized with '|' as level delimiter and summation symbols
#'          for grouping subcategories into entities e.g. for stackplots.
#'          Notice the following hints for the summation symbol placement:
#' \itemize{
#'   \item Every name should just contain one summation symbol (mostly '+').
#'   \item The position of the symbol (counted in '|' from left side) will determine the level.
#'   \item Every subitem containing the same summation symbol in the same level
#'         with the same supercategory name will be summed.
#'   \item Items without any summation symbol will be silently ignored.
#'   \item Items with different summation symbols will be summed up separately.
#'   \item In most of the cases a summation symbol will be just placed
#'         before the last level (counted in '|' from left side).
#'   \item It is helpful to think about which group of items should be stacked in a stackplot.
#' }
#'   An example how a summation symbol placement could look like:
#'   \preformatted{  Toplevel
#'   Toplevel|+|Item 1
#'   Toplevel|+|Item 2
#'   Toplevel|Item 2|+|Subitem 1
#'   Toplevel|Item 2|+|Subitem 1
#'   Toplevel|++|Item A
#'   Toplevel|++|Item B
#'   Toplevel|Item ?}
#'
#' @author Florian Humpenoeder, David Klein
#' @importFrom magclass write.report2 getSets add_dimension
#' @importFrom methods is
#' @examples
#' \dontrun{
#' x <- getReportMAgPIE2REMIND(gdx)
#' }
#'
getReportMAgPIE2REMIND <- function(gdx, file = NULL, scenario = NULL) {
                      

  message("Start getReportMAgPIE2REMIND(gdx)...")

  t <- system.time(
    output <- tryList("reportDemandBioenergy(gdx,detail=TRUE)",
      "reportEmissions(gdx)",
      "reportCosts(gdx)",
      "reportPriceBioenergy(gdx)",
      gdx = gdx
    )
  )

  message(paste0("Total runtime:  ", format(t["elapsed"], nsmall = 2, digits = 2), "s"))

  output <- .filtermagpie(mbind(output), gdx, filter = c(1, 2, 7))

  getSets(output, fulldim = FALSE)[3] <- "variable"

  if (!is.null(scenario)) {
    output <- add_dimension(output,
      dim = 3.1,
      add = "scenario",
      nm = gsub(".", "_", scenario, fixed = TRUE)
    )
  }
  output <- add_dimension(output, dim = 3.1, add = "model", nm = "MAgPIE")

  missingUnit <- !grepl("\\(.*\\)", getNames(output))
  if (any(missingUnit)) {
    warning("Some units are missing in getReportMAgPIE2REMIND!")
    warning("Missing units in:", getNames(output)[which(!grepl("\\(.*\\)", getNames(output)) == TRUE)])
    getNames(output)[missingUnit] <- paste(getNames(output)[missingUnit], "( )")
  }
  
  if (!is.null(file)) {
    write.report2(output, file = file)
  }
  
  return(invisible(output))
}
