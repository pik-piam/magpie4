test_that("a full report can be generated without errors or warnings", {
  skip_on_cran()

  fixturesDir <- "tmp_fixtures"
  gdxPath <- file.path(fixturesDir, "magpie-default-fulldata.gdx")
  skip_if_not(file.exists(gdxPath))

  # Run getReport and check for error messages
  report <- NULL
  expect_no_warning(
    expect_no_message(
      report <- getReport(gdxPath),
      message = "ERROR .*"
    )
  )

  # Verify that report was generated and is not empty
  expect_true(is.magpie(report))
  expect_true(length(report) > 0)
})