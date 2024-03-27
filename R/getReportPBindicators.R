#' @title getReportPBindicators
#' @description Puts together all reporting variables for planetary boundary indicators
#'              of MAgPIE
#' @export
#'
#' @param gdx      GDX file
#' @param file     a file name the output should be written to using write.report.
#'                 If NULL the report is returned instead as a MAgPIE object.
#' @param scenario Name of the scenario used for the list-structure of a
#'                 reporting object (x$scenario$MAgPIE).
#'                 If NULL the report is returned instead as a MAgPIE object.
#' @param filter   Modelstat filter. Here you have to set the modelstat values
#'                 for which results should be used.
#'                 All values for time steps in which the modelstat is different
#'                 or for which one of the previous modelstats were different are set to NA.
#' @param dir      for gridded intermediate outputs: magpie output directory
#'                 which contains a mapping file (rds or spam)
#' @param ...      additional arguments for write.report.
#'                 Will only be taken into account if argument "file" is not NULL.
#' @return A MAgPIE object containing the report in the case that "file" is NULL.
#' @details Reports are organize with '|' as level delimiter and summation symbols
#'          for grouping subcategories into entities e.g. for stackplots.
#'          Notice the following hints for the summation symbol placement:
#' \itemize{
#'   \item Every name should just contain one summation symbol (mostly '+').
#'   \item The position of the symbol (counted in '|' from left side) will determine the level.
#'   \item Every subitem containing the same summation symbol in the same level
#'         with the same supercategory name will be summed.
#'   \item Items without any summation symbol will ge ignored.
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
#' @author Felicitas Beier
#' @importFrom magclass write.report2 getSets add_dimension
#' @importFrom methods is
#' @examples
#' \dontrun{
#' x <- getReport(gdx)
#' }
#'
getReportPBindicators <- function(gdx, file = NULL, scenario = NULL, filter = c(1, 2, 7),
                                  dir = ".", ...) {

  message("Start getReportPBindicators(gdx)...")

  t <- system.time(
    output <- tryList(# reportClimate: climate variables are part of remind.mif
                      # or need to be derived from MAGICC
                      "reportPBbiosphere(gdx, dir = dir)",
                      "reportPBland(gdx)",
                      "reportPBwater(gdx)",
                      "reportPBnitrogen(gdx, dir = dir)",
                      gdx = gdx
    )
  )

  message(paste0("Total runtime:  ", format(t["elapsed"], nsmall = 2, digits = 2), "s"))

  output <- .filtermagpie(mbind(output), gdx, filter = filter)

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
    warning("Some units are missing in getReport!")
    warning("Missing units in:", getNames(output)[which(!grepl("\\(.*\\)", getNames(output)) == TRUE)])
    getNames(output)[missingUnit] <- paste(getNames(output)[missingUnit], "( )")
  }
  if (!is.null(file)) {
    write.report2(output, file = file, ...)
  } else {
    return(output)
  }
}
