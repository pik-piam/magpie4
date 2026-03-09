test_that("expectVariablesPresent detects missing variables", {
  report <- magclass::maxample("pop")
  getItems(report, 3) <- c("Population", "OtherPopulation")
  getSets(report)[3] <- "variable"

  expectVariablesPresent(report, c("Population", "OtherPopulation"))
  expectVariablesPresent(report, c("Population", "OtherPopulation", "OtherPopulation"))

  expect_warning(expectVariablesPresent(report, c("Population", "OtherPopulation", "NotAVariable")),
                 "variables are expected in the piamInterfaces package")

  getItems(report, 3) <- c("Population|++|Value", "OtherPopulation|Value")
  expectVariablesPresent(report, c("Population|Value", "OtherPopulation|+|Value"))
})