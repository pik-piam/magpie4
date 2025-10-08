#' @title tryReport
#' @description Internal support function to run a reporting in a try environment
#' and properly report problems if something goes wrong without stopping the
#' further processing in case of an error
#'
#' @param report report function to be run
#' @param width  max number of characters per line
#' @param gdx gdx file to report from
#' @param level spatial level (either "regglo" for region+global, "iso" for country-level, or the file of a mapping file)
#' @param n number of parent generations to go back when catching the environment
#' the report should get evaluated in
#' @author Jan Philipp Dietrich
#' @importFrom gdx2 readGDX

tryReport <- function(report, width, gdx, level = "regglo", n = 1) {
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
  message("   ", format(report, width = width), appendLF = FALSE)
  t <- system.time(x <- try(eval.parent(parse(text = paste0("suppressMessages(", report, ")")), n = 1 + n),
                            silent = TRUE))
  t <- paste0(" ", format(t["elapsed"], nsmall = 2, digits = 2), "s")
  if (is(x, "try-error")) {
    message("ERROR", t)
    x <- NULL
  } else if (is.null(x)) {
    message("no return value", t)
    x <- NULL
  } else if (is.character(x)) {
    message(x, t)
    x <- NULL
  } else if (!is.magpie(x)) {
    message("ERROR - no magpie object", t)
    x <- NULL
  } else if (!setequal(getYears(x), years)) {
    message("ERROR - wrong years", t)
    x <- NULL
  } else if (!setequal(getItems(x, dim = 1), regs) &&
               (is.null(additionalRegs) || !setequal(getItems(x, dim = 1), additionalRegs))) {
    # The condition above should be removed as soon as all sub-reports
    # respect the report level (see comment above).
    message("ERROR - wrong regions", t)
    x <- NULL
  } else if (any(grepl(".", getNames(x), fixed = TRUE))) {
    message("ERROR - data names contain dots (.)", t)
    x <- NULL
  } else {
    message("success", t)
  }
  return(x)
}
