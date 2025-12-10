#' @title OtherLand
#' @description Disaggregation of other land into initial, restored and recovered land based on a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any aggregation level defined in superAggregateX. In addition "climate" for the 3 climate regions tropical, temperate and boreal is available.
#' @details initial, restored and recovered land
#' @return Other land area in Mha
#' @author Florian Humpenoeder
#' @importFrom magclass dimSums collapseNames
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- PeatlandArea(gdx)
#'   }

OtherLand <- function(gdx, level = "reg") {

  a <- dimSums(readGDX(gdx, "ov_land_other", select = list(type = "level"), react = "silent"), dim = "othertype35")
  b <- readGDX(gdx, "pm_land_conservation", react = "silent")
  x <- m_yeardiff(gdx)
  x[, 1, ] <- 5
  ac <- readGDX(gdx, "ac")
  ac_est <- new.magpie("GLO", getYears(x), ac, fill = 0)
  for (t in getYears(x)) {
    ac_est[, t, getNames(ac_est[, t, ])[1:(x[, t, ]/5)]] <- 1
  }
  ac_sub <- 1 - ac_est
  ac_sub[, , "acx"] <- 0

  restoredEst <- collapseDim(b[ , , "other.restore"])
  restoredSub <- collapseDim(as.magpie(apply(b[, , "other.restore"], c(1, 3), cumsum)))
  recoveredEst <- dimSums(a * ac_est, dim = 3) - restoredEst
  recoveredSub <- dimSums(a * ac_sub, dim = 3) - restoredSub

  initial <- setNames(collapseDim(a[, , "acx"]), "other_initial")
  recovered <- setNames(recoveredEst + recoveredSub, "other_recovered")
  restored <- setNames(restoredEst + restoredSub, "other_restored")
  z <- mbind(initial, recovered, restored)

  #check
  indicator <- sum(dimSums(a, dim = 3) - dimSums(z, dim = 3))
  if (abs(indicator) > 1e-6) warning("Other land sub-categories don't add-up to total. Check OtherLand.R function")

  if (level != "cell") z <- superAggregateX(z, aggr_type = "sum", level = level)

  return(z)
}
