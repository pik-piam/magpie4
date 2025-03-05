#' @title lastIter
#' @description Returns the value of a parameter in the last iteration
#'
#' @export
#'
#' @param gdx GDX file
#' @param param Parameter to be returned
#' @param MagpieOutput In inelastic model runs, the food demand model is run once
#' in iter1 based on exo prices prescribed in iter0, then the magpie model in
#' iter1. The magpie results therefore appear in iter1.
#' In elastic model runs, iter2 is started by rerunning the food demand model,
#' but if convergence is reached, MAgPIE does not run again. MAgPIE results
#' are therefore still in iter1, even if the iteration is in iter2.
#'
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' x <- lastIter(gdx)
#' }
#'
lastIter <- function(gdx, param, MagpieOutput = TRUE) {

  allvalue <- readGDX(gdx, param, format = "first_found", react = "silent")
  value <- collapseNames(allvalue[, , "iter1"])

  # we add 1, because the first entry is iter0
  iternumbers <- readGDX(gdx, "p15_iteration_counter") + 1

  if (MagpieOutput == TRUE) {
    # if demand is inelastic, the magpie results from the same period is used,
    # otherwise from the previous period
    s15_elastic_demand <- readGDX(gdx, "s15_elastic_demand")
    sm_fix_SSP2 <- readGDX(gdx, "sm_fix_SSP2")
    elastic <- iternumbers * 0 + s15_elastic_demand
    elastic[, which(getYears(elastic, as.integer = TRUE) <= as.numeric(sm_fix_SSP2)), ] <- 0
    iternumbers <- iternumbers - elastic
  }

  lastiter <- readGDX(gdx, "iter15")[iternumbers]

  for (t in 1:nyears(allvalue)) {
    value[, t, ] <- allvalue[, t, lastiter[t]]
  }

  return(value)
}
