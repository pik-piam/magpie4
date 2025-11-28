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

  tempMapping <- allRegions
  names(tempMapping) <- allRegions
  tempMapping[superRegionRegions] <- "SUPER"
  write.csv(tempMapping, "mymapping.csv")

  expected <- mbind(
    magclass::new.magpie("SUPER", fill = sum(clusterCounts()[superRegionRegions])),
    clusterCounts()[setdiff(allRegions, superRegionRegions), , ]
  )

  # Maps from clusters to the custom mapping
  expect_equal(!!nm(gdxAggregate(testGDXPath, testMagpie(),
                                 to = "mymapping.csv")),
               !!nm(expected))
})