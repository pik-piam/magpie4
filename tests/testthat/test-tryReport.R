test_that("tryReport returns error when report throws an error", {
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i") return(c("AFR", "CPA"))
      if (type == "t") return(c())
    }
  )
  myReportFunction <- function() stop("something went wrong")
  result <- tryReport("myReportFunction()", "")
  expect_equal(result[["type"]], "error")
  expect_match(result[["message"]], "ERROR")
})

test_that("tryReport returns warning when report returns NULL", {
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i") return(c("AFR", "CPA"))
      if (type == "t") return(c())
    }
  )
  myReportFunction <- function() return(NULL)
  result <- tryReport("myReportFunction()", "")
  expect_equal(result[["type"]], "warning")
  expect_equal(result[["message"]], "no return value")
})

test_that("tryReport returns warning when report returns a character vector", {
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i") return(c("AFR", "CPA"))
      if (type == "t") return(c())
    }
  )
  myReportFunction <- function() return("Warning 1")
  result <- tryReport("myReportFunction()", "")
  expect_equal(result[["type"]], "warning")
  expect_equal(result[["message"]], "Warning 1")
})

test_that("tryReport returns warning result when report throws a warning", {
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
  result <- tryReport("myReportFunction()", "")
  expect_equal(result[["type"]], "warning")
  # The result is still returned though
  expect_true(is.magpie(result[["result"]]))
  expect_equal(result[["message"]], "2 warnings, first: Warning 1")
  expect_equal(sapply(result[["warnings"]], conditionMessage), c("Warning 1", "Warning 2"))
})

test_that("tryReport returns validationError when report returns non-magpie object", {
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i") return(c("AFR", "CPA"))
      if (type == "t") return(c())
    }
  )
  myReportFunction <- function() return(42L)
  result <- tryReport("myReportFunction()", "")
  expect_equal(result[["type"]], "validationError")
  expect_equal(result[["message"]], "ERROR - no magpie object")
})

test_that("tryReport returns validationError when report returns magpie with wrong years", {
  withr::local_dir(withr::local_tempdir())
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i") return(c("AFR", "CPA"))
      if (type == "t") return(c("y2020", "y2030"))
    }
  )
  myReportFunction <- function() return(new.magpie(c("AFR", "CPA", "GLO"), years = c("y2025"), fill = 1))
  result <- tryReport("myReportFunction()", "")
  expect_equal(result[["type"]], "validationError")
  expect_equal(result[["message"]], "ERROR - wrong years")
})

test_that("tryReport validates the regions returned by a report function", {

  # regglo
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i" || type == "iso") {
        return(c("AFR", "CPA"))
      } else if (type == "t") {
        return(c())
      }
    }
  )
  ## Correct
  myReportFunction <- function() return(new.magpie(c("AFR", "CPA", "GLO")))
  expect_equal(tryReport("myReportFunction()", "")[["type"]], "success")

  ## Incorrect
  myReportFunction <- function() return(new.magpie(c("AFR", "GLO")))
  result <- tryReport("myReportFunction()", "")
  expect_equal(result[["type"]], "validationError")
  expect_equal(result[["message"]], "ERROR - wrong regions")

  # iso

  ## Correct
  myReportFunction <- function() return(new.magpie(c("AFR", "CPA")))
  expect_equal(tryReport("myReportFunction()", "", level = "iso")[["type"]], "success")

  ## Incorrect
  myReportFunction <- function() return(new.magpie(c("AFR", "CPA", "GLO")))
  result <- tryReport("myReportFunction()", "", level = "iso")
  expect_equal(result[["type"]], "validationError")
  expect_equal(result[["message"]], "ERROR - wrong regions")


  # Custom Mapping
  withr::local_dir(withr::local_tempdir())

  tempMapping <- "RegionCode;NewRegionCode
AFR;AFR
CPA;CPA
AFR;REG1
CPA;REG1"
  writeLines(tempMapping, "mymapping.csv")

  ## Correct
  myReportFunction <- function() return(new.magpie(c("AFR", "CPA", "REG1")))
  expect_equal(tryReport("myReportFunction()", "", level = "mymapping.csv")[["type"]], "success")

  ## Correct, as report functions are currently allows to ignore the passed level
  myReportFunction <- function() return(new.magpie(c("AFR", "CPA", "GLO")))
  expect_equal(tryReport("myReportFunction()", "", level = "mymapping.csv")[["type"]], "success")

  ## Incorrect
  myReportFunction <- function() return(new.magpie(c("A", "REG1")))
  result <- tryReport("myReportFunction()", "", level = "mymapping.csv")
  expect_equal(result[["type"]], "validationError")
  expect_equal(result[["message"]], "ERROR - wrong regions")
})

test_that("tryReport returns validationError when report returns magpie with names containing dots", {
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i") return(c("AFR", "CPA"))
      if (type == "t") return(c())
    }
  )
  myReportFunction <- function() return(new.magpie(c("AFR", "CPA", "GLO"), names = "name.with.dot", fill = 1))
  result <- tryReport("myReportFunction()", "")
  expect_equal(result[["type"]], "validationError")
  expect_equal(result[["message"]], "ERROR - data names contain dots (.)")
})

test_that("tryReport returns success for valid magpie report", {
  local_mocked_bindings(
    readGDX = function(x, type) {
      if (type == "i") return(c("AFR", "CPA"))
      if (type == "t") return(c())
    }
  )
  myReportFunction <- function() return(new.magpie(c("AFR", "CPA", "GLO")))
  result <- tryReport("myReportFunction()", "")
  expect_equal(result[["type"]], "success")
  expect_true(is.magpie(result[["result"]]))
})
