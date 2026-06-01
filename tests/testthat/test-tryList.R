test_that("tryList prints the warning result message from tryReport", {
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i") return(c("AFR", "CPA"))
      if (type == "t") return(c())
    }
  )

  myReportFunction <- function() {
    warning("my warning message")
    return(new.magpie(c("AFR", "CPA", "GLO")))
  }

  printedMessages <- c()

  withCallingHandlers(
    suppressWarnings(tryList("myReportFunction()", gdx = "")),
    message = function(m) {
      printedMessages <<- c(printedMessages, conditionMessage(m)) #nolint: undesireable_operator_linter
      invokeRestart("muffleMessage")
    }
  )

  expect_true(any(grepl("1 warnings, first: my warning message", printedMessages)))
})

test_that("tryList rethrows one warning per warning thrown inside a report function", {
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i") return(c("AFR", "CPA"))
      if (type == "t") return(c())
    }
  )

  myReportFunction <- function() {
    warning("Warning 1")
    warning("Warning 2")
    return(new.magpie(c("AFR", "CPA", "GLO")))
  }

  caughtWarnings <- c()

  withCallingHandlers(
    suppressMessages(tryList("myReportFunction()", gdx = "")),
    warning = function(w) {
      caughtWarnings <<- c(caughtWarnings, conditionMessage(w)) #nolint: undesireable_operator_linter
      invokeRestart("muffleWarning")
    }
  )

  expect_length(caughtWarnings, 2)
  expect_true(any(grepl("Warning 1", caughtWarnings)))
  expect_true(any(grepl("Warning 2", caughtWarnings)))
})
