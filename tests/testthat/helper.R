#
# Custom Expectations
#

expectReportSucceeds <- function(reportFunction, fullDataName = "magpie-default", ...) {
  skip_on_cran()

  gdxPath <- fullDataGdxPath(fullDataName)
  skip_if_not(file.exists(gdxPath), "gdx file not available")

  # Run getReport and check for error messages
  report <- NULL
  expect_no_warning(report <- reportFunction(gdxPath, ...))
  return(report)
}

expectValidReport <- function(report) {
  # This follows the full expectations structure to ensure that we
  # get a useful error message.
  actualReport <- testthat::quasi_label(rlang::enquo(report))

  # Verify that report was generated and is not empty
  if (!is.magpie(actualReport$val)) {
    testthat::fail(c(
      sprintf("Expected %s to result in a magpie object.", actualReport$lab),
      sprintf("But was: %s", deparse(actualReport$val))
    ))
  } else if (!length(actualReport$val) > 0) {
    testthat::fail(
      sprintf("Expected magpie object resulting from %s to contain results, but was empty.", actualReport$lab)
    )
  } else {
    testthat::pass()
  }

  return(actualReport$val)
}

expectEmptyOrValidReport <- function(report) {
  # This follows the full expectations structure to ensure that we
  # get a useful error message.
  actualReport <- testthat::quasi_label(rlang::enquo(report))

  isEmpty <- is.null(actualReport$val) || length(actualReport$val) == 0
  isValid <- is.magpie(actualReport$val) && length(actualReport$val) > 0
  if (!(isEmpty || isValid)) {
    testthat::fail(
      sprintf("Expected %s to result in either a valid or empty result, but was neither but instead %s.",
              actualReport$lab,
              deparse(actualReport$val))
    )
  } else {
    testthat::pass()
  }

  return(actualReport$val)
}

expectDisabledReport <- function(report) {
  # This follows the full expectations structure to ensure that we
  # get a useful error message.
  actualReport <- testthat::quasi_label(rlang::enquo(report))

  if (!grepl("Disabled.*", actualReport$val)) {
    testthat::fail(sprintf(
      "Expected report %s to be disabled, but got %s instead.",
      actualReport$lab,
      deparse(actualReport$val)
    ))
  } else {
    testthat::pass()
  }

  return(actualReport$val)
}

oldAndCurrentData <- function() {
  return(c("magpie-default", "magpie-old-default"))
}


#
# Fixture Helpers
#

setupFullDataNamed <- function(fullDataName = "magpie-default") {

  fullDataFileName <- paste0(fullDataName, ".tar.gz")

  # Setup paths
  fullDataServer <- "https://rse.pik-potsdam.de/data/example/magpie-fulldata/"
  fullDataUrl <- paste0(fullDataServer, fullDataFileName)
  fixturesDir <- "tmp_fixtures"
  fullDataTargetPath <- file.path(fixturesDir, fullDataFileName)
  fullDataTargetFolder <- file.path(fixturesDir, fullDataName)

  # Create fixtures directory if it doesn't exist
  if (!dir.exists(fixturesDir)) {
    dir.create(fixturesDir, recursive = TRUE)
  }

  # Only one of the setup scripts should do this, so we create a lock.
  # We wait for that lock indefinitely, so that we can only proceed once the
  # setup has been completed by one process.
  setupLock <- filelock::lock(file.path(fixturesDir, paste0(fullDataName, ".lock")),
                              timeout = Inf)

  tryCatch(
    {
      # Download tar.gz file if target folder doesn't exist yet or is older than 36 hours
      shouldDownload <- FALSE
      if (!file.exists(fullDataTargetFolder)) {
        shouldDownload <- TRUE
      } else {
        fileInfo <- file.info(fullDataTargetFolder)
        fileAge <- difftime(Sys.time(), fileInfo$mtime, units = "hours")
        if (fileAge > 36) {
          shouldDownload <- TRUE
        }
      }

      if (shouldDownload) {
        withr::local_options(timeout = 10 * 60) # 10 Minutes timeout
        utils::download.file(fullDataUrl, fullDataTargetPath, mode = "wb", quiet = TRUE)
        utils::untar(fullDataTargetPath, exdir = fixturesDir)
        file.remove(fullDataTargetPath)
      }
    },
    finally = {
      filelock::unlock(setupLock)
    }
  )

}

fullDataGdxPath <- function(fullDataName = "magpie-default") {
  return(file.path("tmp_fixtures", fullDataName, "fulldata.gdx"))
}
