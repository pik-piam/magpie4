test_that("tryReport validates the regions returned by a report function", {
  withr::local_dir(withr::local_tempdir())

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
