#' @title tryReport
#' @description Internal support function to run a reporting in a try environment
#' and properly report problems if something goes wrong without stopping the
#' further processing in case of an error
#'
#' @param report report function to be run
#' @param width  max number of characters per line
#' @param gdx gdx file to report from
#' @param level spatial level (either "regglo" for region+global or "iso" for ISO countries)
#' @param n number of parent generations to go back when catching the environment
#' the report should get evaluated in
#' @author Jan Philipp Dietrich
#' @importFrom gdx2 readGDX

tryReport <- function(report, width, gdx, level = "regglo", n = 1) {
  if (level == "regglo") {
    regs <- c(readGDX(gdx, "i"), "GLO")
  } else if (level == "iso") {
    regs <- readGDX(gdx, level)
  } else {
    # TODO: Fix aggregation check
    regs <- NULL
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
  } else if (!is.null(regs) && !setequal(getItems(x, dim = 1), regs)) {
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
