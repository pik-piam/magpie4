testGDXPath <- "fixtures/test.gdx"

testMagpie <- function() {
  g <- gdx2::readGDX(testGDXPath)
  return(magclass::new.magpie(g$cell$j, fill = 1))
}

clusterCounts <- function() {
  return(magclass::dimSums(testMagpie(), dim = 1.2))
}

nm <- function(x) {
  getComment(x) <- NULL
  getSets(x, fulldim = TRUE) <- c("d1", "d2", "d3")
  return(x)
}

test_that("gdxAggregate unweighted aggregation", {
  expect_equal(nm(gdxAggregate(testGDXPath, testMagpie(), to = "reg")),
               nm(clusterCounts()))
})

test_that("gdxAggregate weighted aggregation", {
  weights <- testMagpie()
  weights["IND.48", , ] <- 2
  m <- testMagpie()
  m["IND.48", , ] <- 5 # With that IND adds up to 2 * sum(weights[IND])

  expected <- clusterCounts()
  expected[, , ] <- 1
  expected["IND", , ] <- 2

  expect_equal(!!nm(gdxAggregate(testGDXPath, m,
                                 weight = weights, to = "reg", absolute = FALSE)),
               !!nm(expected))
})

test_that("gdxAggregate with custom mapping", {
  allRegions <- getItems(clusterCounts(), 1)
  superRegionRegions <- c("CAZ", "SSA", "JPN")

  # Create custom mapping
  tmpMappingFile <- withr::local_tempfile(fileext = ".csv")
  tempMapping <- data.frame(
    reg = c(allRegions, superRegionRegions),
    to  = c(allRegions, rep("SUPER", 3))
  )
  write.csv(tempMapping, tmpMappingFile, row.names = FALSE)

  expected <- mbind(
    clusterCounts(),
    magclass::new.magpie("SUPER", fill = sum(clusterCounts()[superRegionRegions]))
  )

  # Maps from clusters to the custom mapping
  expect_equal(!!nm(gdxAggregate(testGDXPath, testMagpie(),
                                 to = tmpMappingFile)),
               !!nm(expected))

  # Custom mapping aggregation is idempotent
  expect_equal(!!nm(gdxAggregate(testGDXPath,
                                 gdxAggregate(testGDXPath, testMagpie(), to = tmpMappingFile),
                                 to = tmpMappingFile)),
               !!nm(expected))
})

test_that("gdxAggregate on aggregated data", {
  expect_equal(nm(gdxAggregate(testGDXPath, 
                               gdxAggregate(testGDXPath, testMagpie(), to = "reg"),
                               to = "reg")),
               nm(clusterCounts()))
})