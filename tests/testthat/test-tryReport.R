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
  myReportFunction <<- function() return(new.magpie(c("AFR", "CPA", "GLO")))
  expect_message(tryReport("myReportFunction()", width = 30, ""), "success")

  ## Incorrect
  myReportFunction <<- function() return(new.magpie(c("AFR", "GLO")))
  expect_message(tryReport("myReportFunction()", width = 30, ""), "ERROR - wrong regions")

  # iso

  ## Correct
  myReportFunction <<- function() return(new.magpie(c("AFR", "CPA")))
  expect_message(tryReport("myReportFunction()", width = 30, "", level = "iso"), "success")

  ## Incorrect
  myReportFunction <<- function() return(new.magpie(c("AFR", "CPA", "GLO")))
  expect_message(tryReport("myReportFunction()", width = 30, "", level = "iso"), "ERROR - wrong regions")


  # Custom Mapping
  tempMapping <- "RegionCode;NewRegionCode
AFR;AFR
CPA;CPA
AFR;REG1
CPA;REG1"
  writeLines(tempMapping, "mymapping.csv")

  ## Correct
  myReportFunction <<- function() return(new.magpie(c("AFR", "CPA", "REG1")))
  expect_message(tryReport("myReportFunction()", width = 30, "", level = "mymapping.csv"), "success")

  ## Correct, as report functions are currently allows to ignore the passed level
  myReportFunction <<- function() return(new.magpie(c("AFR", "CPA", "GLO")))
  expect_message(tryReport("myReportFunction()", width = 30, "", level = "mymapping.csv"), "success")

  ## Incorrect
  myReportFunction <<- function() return(new.magpie(c("A", "REG1")))
  expect_message(tryReport("myReportFunction()", width = 30, "", level = "mymapping.csv"), "ERROR - wrong regions")
})
