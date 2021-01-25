#' @title PrimSecdOtherLand
#' @description Calculates share of primary and secondary non-forest vegetation for different aggregation levels based on gridded magpie output and luh2v2 initial shares of primary and secondary non-forest vegetation.
#'
#' @importFrom magclass mbind read.magpie dimSums
#' @importFrom luscale superAggregate
#' @importFrom gdx out
#' @export
#'
#' @param land_share Cellular magpie output file, containg share of different land use in magpie
#' @param file a file name the output should be written to using \code{write.magpie}
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate. The unit of output for the cases above is Mha. If level "grid" is specified the unit of output can be chosen between "share" and "Mha".
#' @param dir output directory which contains cellular magpie output and the the luh2v2 initialisation data for primary and secondary vegetation ("cell.luh2v2initial_primsecdother_0.5_share.mz").
#' @param unit_gridded "Mha" or "share". Defines the unit of the gridded output, see also \code{level}.
#' @return Area of primary and secondary non-forested vegetation in MAgPIE (other land) as MAgPIE object; either as unit of area (Mha) or as fraction of total land per grid cell (share).
#' @author Patrick v. Jeetze, Kristine Karstens
#' @seealso \code{\link{MapLandUseForDownscaling}}
#' @examples
#'
#' \dontrun{
#' x <- PrimSecdOtherLand(dir = ".")
#' }
#'
PrimSecdOtherLand <- function(land_share = "cell.land_0.5_share.mz", file = NULL, level = "grid", dir = ".", unit_gridded = "share") {
  
  landShr_mgp <- read.magpie(file.path(dir, land_share))
  otherShr_mgp <- landShr_mgp[, , "other"]

  # calculate the change (difference) of other land over time per grid cell
  dOther <- otherShr_mgp[, 2:nyears(otherShr_mgp), ] - setYears(otherShr_mgp[, 1:nyears(otherShr_mgp) - 1, ], tail(getYears(otherShr_mgp), -1))

  primSecdOther_luh <- setCells(read.magpie(file.path(dir, "cell.luh2v2initial_primsecdother_0.5_share.mz")), getCells(landShr_mgp))
  totOther_luh <- primSecdOther_luh[, , "primn"] + primSecdOther_luh[, , "secdn"]

  # calc shares out of LUH data
  primOtherShr <- totOther_luh
  primOtherShr[totOther_luh > 0, , ] <- primSecdOther_luh[totOther_luh > 0, , "primn"] / setNames(totOther_luh[totOther_luh > 0, , ], "primn")
  secOtherShr <- totOther_luh
  secOtherShr[totOther_luh > 0, , ] <- primSecdOther_luh[totOther_luh > 0, , "secdn"] / totOther_luh[totOther_luh > 0, , ]
  # primOtherShr, secOtherShr will be 0 for totOther_luh is zero
  # no shares if other land is nonzero in magpie initial
  # a proxy is needed for all cells that are unequal zero in magpie init but zero in luh 1995
  # here the simple assumption that this other land is pure secOther land
  secOtherShr[totOther_luh == 0 & otherShr_mgp[, 1, ] > 0] <- 1

  # set initial total other land
  totOther <- otherShr_mgp[, 1, ]
  # set initial primary vegetation
  primOther <- setYears(primOtherShr * totOther, "y1985")
  primOther <- setNames(primOther, getNames(totOther))
  # set initial secondary vegetation
  secOther <- setYears(secOtherShr * totOther, "y1985")
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

    primTemp[secTemp < 0, , ] <- primTemp[secTemp < 0, , ] + secTemp[secTemp < 0, , ]

    # tiny (e-16) negative values can occur because of rounding ?
    primTemp[primTemp < 0, , ] <- 0
    secTemp[secTemp < 0, , ] <- 0

    primOther <- mbind(primOther, primTemp)
    secOther <- mbind(secOther, secTemp)
    totTemp <- primTemp + secTemp
    totOther <- mbind(totOther, totTemp)
  }

  primSecOther <- mbind(setNames(primOther, "primother"), setNames(secOther, "secdother"))

  if (level == "grid") {
    if (unit_gridded == "Mha") {
      cell_area <- read.magpie(file.path(dir, "cell.land_0.5.mz"))
      cell_area <- dimSums(cell_area[, 1, ], dim = 3)
      primSecOther <- setYears(primSecOther * cell_area, getYears(primSecOther))
    } else {}
  }
  else {
    cell_area <- read.magpie(file.path(dir, "cell.land_0.5.mz"))
    cell_area <- dimSums(cell_area[, 1, ], dim = 3)
    primTemp <- superAggregate(setYears(primSecOther[, , "primother"] * cell_area, getYears(primSecOther)), level = tolower(level), aggr_type = "sum")
    secTemp <- superAggregate(setYears(primSecOther[, , "secdother"] * cell_area, getYears(primSecOther)), level = tolower(level), aggr_type = "sum")
    primSecOther <- mbind(primTemp, secTemp)
  }

  out(primSecOther, file)
}
