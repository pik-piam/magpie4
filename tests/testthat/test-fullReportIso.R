test_that("a iso report can be generated without errors or warnings", {
  skip_on_cran()
  run_only_if_full_tests_requested()

  gdxPath <- fullDataGdxPath()
  skip_if_not(file.exists(gdxPath))

  # Run getReport and check for error messages
  report <- NULL
  expect_no_warning(
    expect_no_message(
      report <- getReportIso(gdxPath),
      message = "ERROR .*"
    )
  )

  expectValidReport(report)
})
