#' addToDataChangelog
#'
#' Prepend data from the given report to the changelog.
#'
#' @param report data.frame as obtained by readRDS("report.rds")
#' @param changelog Path to the changelog file
#' @param versionId The model version identifier, e.g. a release number like 4.9.1 or a date like 2025-02-01
#' @param years For which years the variables should be read and put into the changelog
#' @param variables Which variables to read from the report (e.g. "Emissions|CO2|Land|+|Land-use Change")
#' @param ... Reserved for future expansion.
#' @param maxEntries The maximum number of versionIds to keep in the changelog, the oldest one is removed first.
#' @param roundDigits Numbers are rounded to this many decimal places before being written to the changelog.
#' @return Invisibly, the written changelog as data.frame
#'
#' @author Pascal Sauer
#' @export
addToDataChangelog <- function(report, changelog, versionId, years, variables, ...,
                               maxEntries = 15, roundDigits = 2) {
  x <- report[report$region == "World"
              & report$variable %in% variables
              & report$period %in% years,
              c("variable", "period", "value")]
  notFound <- setdiff(variables, x$variable)
  if (length(notFound) > 0) {
    warning("No data-changelog data found for ", paste(notFound, collapse = ", "))
  }

  # shorten variable names
  variableNames <- names(variables)
  names(variableNames) <- variables
  x$variable <- variableNames[as.character(x$variable)]

  colnames(x)[ncol(x)] <- versionId

  out <- data.frame(version = setdiff(colnames(x), c("variable", "period")))
  for (variableName in variableNames) {
    for (year in years) {
      newColumn <- x[x$variable == variableName & x$period == year, 3]
      newColumn <- round(newColumn, roundDigits)
      out <- cbind(out, newColumn)
      colnames(out)[ncol(out)] <- paste0(variableName, year)
    }
  }

  if (file.exists(changelog)) {
    changelog <- normalizePath(changelog)
    xChangelog <- read.csv(changelog)

    # add columns only existing in out/xChangelog to the other
    newCols <- setdiff(colnames(out), colnames(xChangelog))
    oldCols <- setdiff(colnames(xChangelog), colnames(out))
    naList <- function(listNames) {
      x <- rep(NA, length(listNames))
      names(x) <- listNames
      return(as.list(x))
    }
    out <- cbind(out, naList(oldCols))
    xChangelog <- cbind(xChangelog, naList(newCols))

    out <- rbind(out, xChangelog)
    out <- out[seq_len(min(maxEntries, nrow(out))), ]
  }
  write.csv(out, changelog, quote = FALSE, row.names = FALSE)
  return(invisible(out))
}
