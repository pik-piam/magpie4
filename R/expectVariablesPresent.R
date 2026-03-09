#' @title expectVariablesPresent
#' @description Checks whether a set of expected variable names is present in a
#'   MAgPIE report object. Variable names are normalized in that summation symbols
#'   are removed.
#'
#' @param report A MAgPIE object containing the report to check.
#' @param variableNames A character vector of variable names that are expected
#'   to be present in \code{report}.
#' @return \code{NULL} invisibly. Issues a warning if expected variables are not present.
#' @author Patrick v. Jeetze
#' @importFrom magclass getNames
#' @importFrom piamutils deletePlus
#' @export
expectVariablesPresent <- function(report, variableNames) {
  missingVariables <- sort(setdiff(unique(deletePlus(variableNames)),
                                   unique(deletePlus(getNames(report, dim = "variable")))))
  if (length(missingVariables) > 0) {
    warning("# The following ", length(missingVariables), " variables are expected in the piamInterfaces package ",
            "but cannot be found in the MAgPIE report.\n",
            "Please either fix in magpie4 or adjust the mapping in piamInterfaces.\n- ",
            paste(missingVariables, collapse = ",\n- "), "\n")
  }
}
