#' @title tc
#' @description Calculates TC rates based on a MAgPIE gdx file
#'
#' @importFrom magclass getRegions
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional
#' and global) or any other aggregation level defined in superAggregate
#' @param annual If TRUE, annual values are reported. If FALSE, the values for the whole timestep are
#' reported. If FALSE, avrg has no effect
#' @param avrg If FALSE the annual tc rates of the current period are returned, otherwise the average annual
#' tc rate for the period tbase to tn is returned. tbase defaults to the first timestep (see baseyear)
#' @param baseyear Determines the base year timestep for annual tc calculation. Average tc rates for later
#' timesteps are calculated with respect to baseyear. No tc rates for timesteps before baseyear are returned)
#' @return A MAgPIE object containing tc rates. Annual ones if annual=TRUE, for the whole timestep if annual=FALSE.
#' @author Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' x <- tc(gdx)
#' }
#'
tc <- function(gdx, file = NULL, level = "reg", annual = TRUE, avrg = FALSE, baseyear = 1995) {
  x <- setNames(readGDX(gdx, "ov_yld_tc", "ovm_yld_tc", format = "first_found", react = "silent")[, , "level"], NULL)
  if (is.null(x)) {
    tau <- tau(gdx, start_value = TRUE, digits = 6)
    if (is.null(tau)) {
      warning("TC cannot be calculated as TC data could not be found in GDX file! NULL is returned!")
      return(NULL)
    }
    x <- tau
    if (nyears(x) > 2) {
      x[, 2:nyears(x), 1] <- tau[, 2:nyears(x), 1] / setYears(tau[, (2:nyears(x)) - 1, 1], getYears(x)[2:nyears(x)]) - 1
    }
    x <- x[, 2:nyears(x), ]
  }
  if (annual) {
    # correct tc values so that one gets annual values
    y <- getYears(x, as.integer = TRUE)
    l <- y
    l[1] <- 1
    if (length(y) > 1) l[2:length(l)] <- y[2:length(y)] - y[1:(length(y) - 1)]
    names(l) <- getYears(x)
    tc <- x
    for (i in getYears(x)) {
      tc[, i, ] <- (1 + x[, i, ]) ^ (1 / l[i]) - 1
    }
  } else {
    tc <- x
  }
  if (level != "reg") {
    cr <- croparea(gdx, level = "reg", water_aggr = TRUE)
    if (is.null(cr)) {
      warning("TC data cannot be aggregated as croparea function returned NULL! NULL is returned!")
      return(NULL)
    }
    tc <- superAggregateX(tc, aggr_type = "weighted_mean", level = level, weight = cr)
  }
  if (avrg & annual) {
    basepos <- which(getYears(tc, as.integer = TRUE) == baseyear)
    if (length(basepos) == 0) stop("baseyear does not exist in model output")
    if (basepos == nyears(tc)) {
      warning("baseyear is the last year of the simulation period. No tc rates are calculated")
      return(NULL)
    }
    tmp <- tc[, (basepos + 1):nyears(tc), ]
    tcAvrg <- tmp
    l <- l[(basepos + 1):length(l)]
    for (r in getRegions(tc)) {
      for (i in 1:nyears(tmp)) {
        tmp[r, i, 1] <- (1 + tc[r, basepos + i, 1])^l[i]
        tcAvrg[r, i, 1] <- (prod(tmp[r, 1:i, 1])) ^ (1 / sum(l[1:i])) - 1
      }
    }
    tc <- tcAvrg
  }
  out(round(tc, 4), file)
}
