test_that("addGeometry is independent of order", {
  x <- new.magpie(c("CHA.15", "CHA.16", "CHA.17"))
  mapping <- data.frame(cluster = c("CHA.15", "CHA.15", "CHA.15", "CHA.16", "CHA.16",
                                    "CHA.17", "CHA.17", "CHA.17", "CHA.17", "CHA.17"),
                        cell = c("81p25.38p25", "82p25.40p25", "84p75.39p25", "105p75.25p75",
                                 "120p75.24p75", "81p25.31p25", "81p75.31p25", "91p25.33p25",
                                 "91p75.36p25", "96p75.37p75"))
  xWithGeometry <- addGeometry(x, mapping)

  mapping <- mapping[10:1, ]
  xWithGeometry2 <- addGeometry(x, mapping)
  expect_identical(xWithGeometry, xWithGeometry2)

  x <- x[3:1, , ]
  xWithGeometry3 <- addGeometry(x, mapping)
  expect_identical(attr(xWithGeometry3, "geometry"),
                   attr(xWithGeometry, "geometry")[3:1])
})
