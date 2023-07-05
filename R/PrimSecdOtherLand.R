#' @title PrimSecdOtherLand
#' @description Calculates share of primary and secondary non-forest vegetation for different aggregation levels based on gridded magpie output and  initial shares of primary and secondary non-forest vegetation.
#'
#' @importFrom magclass mbind read.magpie dimSums
#' @importFrom luscale superAggregate
#' @importFrom gdx out
#' @export
#'
#' @param x Time series of land pools (model output) containing only one aggregated class for other land. Can be a file or magclass object.
#' @param ini_file Initialisation file for primary and secondary other land (e.g. based on 1995 MAgPIE land-use initialisation values). Must have the same spatial resolution as \code{x}.
#' @param ini_year Reference year for estimating primary and secondary other land shares, must be included in \code{ini_file}.
#' @param file a file name the output should be written to using \code{write.magpie}
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate. The unit of output for the cases above is Mha. If level "grid" is specified the unit of output can be chosen between "share" and "Mha".
#' @param unit "Mha" or "share". Defines the unit of the gridded output, see also \code{level}.
#' @return \code{x} including land area for primary and secondary non-forested vegetation in MAgPIE (other land) as MAgPIE object; either as unit of area (Mha) or as fraction of total land per grid cell (share).
#' @author Patrick v. Jeetze, Kristine Karstens
#' @examples
#' \dontrun{
#' x <- "./cell.land_0.5.nc"
#' land <- PrimSecdOtherLand(x)
#'
#' # direct use of disaggregation output
#' land <- PrimSecdOtherLand(land_hr)
#' }
#'
PrimSecdOtherLand <- function(x, ini_file, ini_year = "y1995", file = NULL, level = "grid", unit = "Mha") {
  if (is.magpie(x)) {
    land <- x[, "y1985", , invert = TRUE]
  } else {
    land <- read.magpie(x)[, "y1985", , invert = TRUE]
  }
  otherLand <- land[, , "other"]

  if (is.magpie(ini_file)) {
    primSecIni <- ini_file[, ini_year, ]
  } else {
    primSecIni <- read.magpie(ini_file)[, ini_year, ]
  }

  # calculate the change (difference) of other land over time per grid cell
  dOther <- otherLand[, 2:nyears(otherLand), ] - setYears(otherLand[, 1:nyears(otherLand) - 1, ], tail(getYears(otherLand), -1))

  primSecIni <- setCells(primSecIni, getCells(land))
  totOtherIni <- primSecIni[, , "primother"] + primSecIni[, , "secdother"]

  # calc shares out of initialisation data
  primOtherShr <- totOtherIni
  primOtherShr[totOtherIni > 0, , ] <- primSecIni[totOtherIni > 0, , "primother"] / totOtherIni[totOtherIni > 0, , ]
  primOtherShr <- setNames(primOtherShr, "primother")
  secOtherShr <- totOtherIni
  secOtherShr[totOtherIni > 0, , ] <- primSecIni[totOtherIni > 0, , "secdother"] / totOtherIni[totOtherIni > 0, , ]
  secOtherShr <- setNames(secOtherShr, "secdother")
  # primOtherShr, secOtherShr will be 0 where totOtherIni is zero
  # make sure that cells unequal zero in magpie init but zero in the reference data receive a value
  # here the simple assumption that this other land is pure secondary other land
  secOtherShr[totOtherIni == 0 & otherLand[, 1, ] > 0] <- 1

  # set initial total other land
  totOther <- otherLand[, 1, ]
  # set initial primary vegetation
  primOther <- setYears(primOtherShr * totOther, getYears(otherLand)[1])
  primOther <- setNames(primOther, getNames(totOther))
  # set initial secondary vegetation
  secOther <- setYears(secOtherShr * totOther, getYears(otherLand)[1])
  secOther <- setNames(secOther, getNames(totOther))

  for (j in 1:nyears(dOther)) {
    primCoeff <- primOther[, j, ] / totOther[, j, ]
    primCoeff[is.na(primCoeff), , ] <- 0
    secCoeff <- secOther[, j, ] / totOther[, j, ]
    secCoeff[is.na(secCoeff), , ] <- 0

    # compute change in primary veg for each time step
    primTemp <- setYears(primOther[, j, ], getYears(dOther[, j, ]))
    primTemp[dOther[, j, ] < 0, , ] <- primTemp[dOther[, j, ] < 0, , ] + setYears(primCoeff[dOther[, j, ] < 0, , ], getYears(dOther[, j, ])) * dOther[dOther[, j, ] < 0, j, ]

    # compute change in secondary veg for each time step
    secTemp <- setYears(secOther[, j, ], getYears(dOther[, j, ]))
    secTemp[dOther[, j, ] > 0, , ] <- secTemp[dOther[, j, ] > 0, , ] + dOther[dOther[, j, ] > 0, j, ]
    secTemp[dOther[, j, ] < 0, , ] <- secTemp[dOther[, j, ] < 0, , ] + setYears(secCoeff[dOther[, j, ] < 0, , ], getYears(dOther[, j, ])) * dOther[dOther[, j, ] < 0, j, ]

    # substract residual secondary other land reduction from primary other land
    primTemp[secTemp < 0, , ] <- primTemp[secTemp < 0, , ] + secTemp[secTemp < 0, , ]

    # tiny (e-16) negative values can occur because of rounding ?
    primTemp[primTemp < 0, , ] <- 0
    secTemp[secTemp < 0, , ] <- 0

    primOther <- mbind(primOther, primTemp)
    secOther <- mbind(secOther, secTemp)
    totTemp <- primTemp + secTemp
    totOther <- mbind(totOther, totTemp)
  }

  land_new <- mbind(
    land[, , "other", invert = TRUE],
    setNames(primOther, "primother"),
    setNames(secOther, "secdother")
  )

  if (level == "grid") {
    if (unit == "Share") {
      cell_area <- dimSums(land_new[, 1, ], dim = 3)
      land_new <- setYears(land_new / cell_area, getYears(land_new))
    }
  } else {
    land_new <- superAggregate(land_new, level = tolower(level), aggr_type = "sum")
    if (unit == "Share") {
      area <- dimSums(land_new[, 1, ], dim = 3)
      land_new <- setYears(land_new / area, getYears(land_new))
    }
  }

  out(land_new, file)
}
