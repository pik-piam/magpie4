expectReportSucceeds <- function(reportFunction, gdxName = "magpie-default-fulldata.gdx", ...) {
  skip_on_cran()
  fixturesDir <- "tmp_fixtures"
  gdxPath <- file.path(fixturesDir, gdxName)
  skip_if_not(file.exists(gdxPath), "gdx file not available")

  # Run getReport and check for error messages
  report <- NULL
  expect_no_warning(report <- reportFunction(gdxPath, ...))
  return(result)
}

expectValidReport <- function(report) {
  # Verify that report was generated and is not empty
  expect_true(is.magpie(report))
  expect_true(length(report) > 0)
}

expectEmptyReport <- function(report) {
  expect_true(is.null(report) || length(report) == 0)
}

expectDisabledReport <- function(report) {
  expect_match(report, "Disabled.*")
}