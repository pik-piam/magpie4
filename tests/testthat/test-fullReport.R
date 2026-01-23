test_that("a full report can be generated without errors or warnings", {
  skip_on_cran()
  run_only_if_full_tests_requested()
  skip_on_covr()

  gdxPath <- fullDataGdxPath()
  skip_if_not(file.exists(gdxPath))

  # Run getReport and check for error messages
  report <- NULL
  expect_no_warning(
    output <- capture_messages(report <- getReport(gdxPath))
  )

  # No report function throws an error
  expect_no_match(paste0(output, collapse = ""),
                  "ERROR .*")

  expectValidReport(report)
})

test_that("a full report for an older fulldata.gdx can be generated without errors or warnings", {
  skip_on_cran()
  run_only_if_full_tests_requested()
  skip_on_covr()

  gdxPath <- fullDataGdxPath("magpie-old-default")
  skip_if_not(file.exists(gdxPath))

  # Run getReport and check for error messages
  report <- NULL
  expect_no_warning(
    output <- capture_messages(report <- getReport(gdxPath))
  )

  # No report function throws an error
  expect_no_match(paste0(output, collapse = ""),
                  "ERROR .*")

  expectValidReport(report)
})
