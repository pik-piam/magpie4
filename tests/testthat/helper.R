expectReportSucceeds <- function(reportFunction, ...) {
  skip_on_cran()
  fixturesDir <- "tmp_fixtures"
  gdxPath <- file.path(fixturesDir, "magpie-default-fulldata.gdx")
  skip_if_not(file.exists(gdxPath))

  # Run getReport and check for error messages
  report <- NULL
  expect_no_warning(report <- reportFunction(gdxPath, ...))

  # Verify that report was generated and is not empty
  expect_true(is.magpie(report))
  expect_true(length(report) > 0)
}