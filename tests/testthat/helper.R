expectReportSucceeds <- function(reportFunction, fullDataFolder = "magpie-default", ...) {
  skip_on_cran()

  gdxPath <- fullDataGdxPath(fullDataFolder)
  skip_if_not(file.exists(gdxPath), "gdx file not available")

  # Run getReport and check for error messages
  report <- NULL
  expect_no_warning(report <- reportFunction(gdxPath, ...))
  return(report)
}

expectValidReport <- function(report) {
  # Verify that report was generated and is not empty
  expect_true(is.magpie(!!report))
  expect_true(!!length(report) > 0)
}

expectEmptyOrValidReport <- function(report) {
  isEmpty <- is.null(report) || length(report) == 0
  isValid <- is.magpie(report) && length(report) > 0
  expect_true(isEmpty || isValid)
}

expectDisabledReport <- function(report) {
  expect_match(report, "Disabled.*")
}

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
  setupLock <- filelock::lock(file.path(fixturesDir, paste0(fullDataName, ".lock")))

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