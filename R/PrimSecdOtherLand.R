#' @title PrimSecdOtherLand
#' @description Calculates the primary and secondary non-forest vegetation (other land) for different aggregation
#' levels from gridded MAgPIE land and the initial shares of primary and secondary other land.
#'
#' @importFrom magclass mbind read.magpie dimSums
#' @export
#'
#' @param x Time series of land pools (model output) with one aggregated class for other land. Can be a file or a
#' magclass object.
#' @param iniFile Initialisation file for primary and secondary other land (e.g. based on 1995 MAgPIE land-use
#' initialisation values). Must have the same spatial resolution as \code{x}.
#' @param iniYear Reference year for the shares of primary and secondary other land, must be included in
#' \code{iniFile}.
#' @param file a file name the output should be written to using \code{write.magpie}
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or
#' any other aggregation level defined in superAggregate. The unit of output for these levels is Mha. For level
#' "grid" the unit of output can be chosen between "share" and "Mha".
#' @param unit "Mha" or "share". Defines the unit of the gridded output, see also \code{level}.
#' @return \code{x} including the land area of primary and secondary non-forest vegetation in MAgPIE (other land)
#' as magclass object; either as area (Mha) or as fraction of the total land per grid cell (share).
#' @author Patrick v. Jeetze, Kristine Karstens
#' @examples
#' \dontrun{
#' x <- "./cell.land_0.5.nc"
#' land <- PrimSecdOtherLand(x)
#'
#' # direct use of disaggregation output
#' land <- PrimSecdOtherLand(landHr)
#' }
#'
PrimSecdOtherLand <- function(x, iniFile, iniYear = "y1995", file = NULL, level = "grid", unit = "Mha") {
  if (is.magpie(x)) {
    land <- x[, "y1985", , invert = TRUE]
  } else {
    land <- read.magpie(x)[, "y1985", , invert = TRUE]
  }
  otherLand <- land[, , "other"]

  if (is.magpie(iniFile)) {
    primSecIni <- iniFile[, iniYear, ]
  } else {
    primSecIni <- read.magpie(iniFile)[, iniYear, ]
  }

  # change of other land per grid cell and time step
  years <- getYears(otherLand)
  dOther <- otherLand[, -1, ] - setYears(otherLand[, -nyears(otherLand), ], years[-1])

  primSecIni <- setCells(primSecIni, getCells(land))
  totOtherIni <- primSecIni[, , "primother"] + primSecIni[, , "secdother"]

  # shares of the initialisation data, 0 where it has no other land
  primOtherShr <- totOtherIni
  primOtherShr[totOtherIni > 0, , ] <- primSecIni[totOtherIni > 0, , "primother"] / totOtherIni[totOtherIni > 0, , ]
  primOtherShr <- setNames(primOtherShr, "primother")
  secOtherShr <- totOtherIni
  secOtherShr[totOtherIni > 0, , ] <- primSecIni[totOtherIni > 0, , "secdother"] / totOtherIni[totOtherIni > 0, , ]
  secOtherShr <- setNames(secOtherShr, "secdother")
  # other land of the first year in grid cells without other land in the initialisation data is secondary
  secOtherShr[totOtherIni == 0 & otherLand[, 1, ] > 0] <- 1

  # initial total, primary and secondary other land; one object per year, bound once after the loop
  totOther <- list(otherLand[, 1, ])
  primOther <- list(setNames(setYears(primOtherShr * totOther[[1]], years[1]), getNames(totOther[[1]])))
  secOther <- list(setNames(setYears(secOtherShr * totOther[[1]], years[1]), getNames(totOther[[1]])))

  for (j in seq_len(nyears(dOther))) {
    change <- dOther[, j, ]
    prim <- setYears(primOther[[j]], getYears(change))
    sec <- setYears(secOther[[j]], getYears(change))
    tot <- setYears(totOther[[j]], getYears(change))
    primCoeff <- prim / tot
    primCoeff[is.na(primCoeff)] <- 0
    secCoeff <- sec / tot
    secCoeff[is.na(secCoeff)] <- 0

    # a loss of other land reduces primary and secondary other land in proportion, a gain adds to secondary
    loss <- change * (change < 0)
    gain <- change * (change > 0)
    primTemp <- prim + primCoeff * loss
    secTemp <- sec + gain + secCoeff * loss

    # a loss of secondary other land beyond its area is taken from primary other land
    primTemp <- primTemp + secTemp * (secTemp < 0)

    # rounding can give tiny (1e-16) negative values
    primTemp[primTemp < 0] <- 0
    secTemp[secTemp < 0] <- 0

    primOther[[j + 1]] <- primTemp
    secOther[[j + 1]] <- secTemp
    totOther[[j + 1]] <- primTemp + secTemp
  }

  landNew <- mbind(land[, , "other", invert = TRUE],
                   setNames(mbind(primOther), "primother"),
                   setNames(mbind(secOther), "secdother"))

  if (level != "grid") {
    landNew <- superAggregateX(landNew, level = tolower(level), aggr_type = "sum")
  }
  if (tolower(unit) == "share") {
    area <- dimSums(landNew[, 1, ], dim = 3)
    landNew <- setYears(landNew / area, getYears(landNew))
  }

  out(landNew, file)
}
