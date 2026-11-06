gridWeight <- function(nCountries = 12, cellsPerCountry = 3, value = 1) {
  iso   <- rep(sprintf("C%02d", seq_len(nCountries)), each = cellsPerCountry)
  cells <- paste0(seq_along(iso), ".", seq_along(iso), ".", iso)
  m <- new.magpie(cells, "y2010", "livst_rum", fill = value)
  getSets(m) <- c("x", "y", "iso", "t", "data")
  m
}

test_that("an extensive weight passes", {
  expect_silent(checkExtensiveLivestockDist(gridWeight(), "f71_livestock_distribution_0.5.mz"))
})

test_that("a weight normalised within country is rejected", {
  shares <- gridWeight(value = 1 / 3)
  expect_error(checkExtensiveLivestockDist(shares, "f71_livestock_distribution_0.5.mz"),
               "normalised within country")
  expect_error(checkExtensiveLivestockDist(shares, "f71_livestock_distribution_0.5.mz"),
               "LivestockDistribution")
})

test_that("too few countries to judge, so no opinion", {
  expect_silent(checkExtensiveLivestockDist(gridWeight(nCountries = 3, value = 1 / 3), "f71.mz"))
})

test_that("a weight without an iso dimension is not judged", {
  expect_silent(checkExtensiveLivestockDist(new.magpie("AAA", "y2010", "livst_rum", fill = 1), "f71.mz"))
})
