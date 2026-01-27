#' @title tryReport
#' @description Internal support function to run a reporting in a try environment
#' and properly report problems if something goes wrong without stopping the
#' further processing in case of an error.
#'
#' @param report report function to be run
#' @param gdx gdx file to report from
#' @param level spatial level (either "regglo" for region+global, "iso" for country-level,
#' or the file of a mapping file).
#' @param env environment to evaluate the report in
#' @return A named list with information on the outcome of the report (success, error, validationError, warning)
#' @author Jan Philipp Dietrich
#' @importFrom gdx2 readGDX
#' @seealso \code{\link{reportResult}}
tryReport <- function(report, gdx, level = "regglo", env = parent.frame()) {
  additionalRegs <- NULL
  if (level == "regglo") {
    regs <- c(readGDX(gdx, "i"), "GLO")
  } else if (level == "iso") {
    regs <- readGDX(gdx, level)
  } else {
    # The following should be changed as soon as all sub-reports
    # respect the report level. The following should then only use
    # the regions from the mapping for validation.
    # (Also remove `additionalRegs` above)
    regs <- c(readGDX(gdx, "i"), "GLO")
    additionalRegs <- tryCatch(
      error = function(err) stop(level, " is neither a valid level nor can a mapping with that name be found."),
      unique(toolGetMapping(level)[, 2])
    )
  }

  years <- readGDX(gdx, "t")
  t <- system.time(x <- try(eval(parse(text = paste0("suppressMessages(", report, ")")), env),
                            silent = TRUE))
  elapsed <- t["elapsed"]

  cond <- if (is(x, "try-error")) {
    reportError(report, elapsed, x)
  } else if (is.null(x)) {
    reportWarning(report, elapsed, "no return value")
  } else if (is.character(x)) {
    reportWarning(report, elapsed, x)
  } else if (!is.magpie(x)) {
    reportValidationError(report, elapsed, "no magpie object")
  } else if (!setequal(getYears(x), years)) {
    reportValidationError(report, elapsed, "wrong years")
  } else if (!setequal(getItems(x, dim = 1), regs) &&
               (is.null(additionalRegs) || !setequal(getItems(x, dim = 1), additionalRegs))) {
    # The condition above should be removed as soon as all sub-reports
    # respect the report level (see comment above).
    reportValidationError(report, elapsed, "wrong regions")
  } else if (any(grepl(".", getNames(x), fixed = TRUE))) {
    reportValidationError(report, elapsed, "data names contain dots (.)")
  } else {
    reportSuccess(report, elapsed, x)
  }

  return(cond)
}

#' @title reportResult
#' @description Create a named list representing the outcome of a report
#' @param resultType Character string for the result type
#' @param message Character string with the result message
#' @param reportExpr The expression containing the call to the report function
#' @param elapsed The elapsed time for the report in seconds
#' @param result The result magpie object or NULL
#' @return A named list with report result information
#' @keywords internal
reportResult <- function(resultType, message, reportExpr, elapsed, result = NULL) {
  elapsedString <- paste0(" ", format(elapsed, nsmall = 2, digits = 2), "s")
  list(
    type = resultType,
    message = message,
    reportExpr = reportExpr,
    elapsed = elapsedString,
    result = result
  )
}

reportSuccess <- function(reportExpr, elapsed, result) {
  reportResult("success", "success", reportExpr, elapsed, result)
}

reportError <- function(reportExpr, elapsed, errorObject) {
  reportResult("error", paste0("ERROR ", errorObject), reportExpr, elapsed)
}

reportValidationError <- function(reportExpr, elapsed, reason) {
  msg <- paste0("ERROR - ", reason)
  reportResult("validationError", msg, reportExpr, elapsed)
}

reportWarning <- function(reportExpr, elapsed, reason) {
  reportResult("warning", reason, reportExpr, elapsed)
}
