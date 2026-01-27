#' @title tryList
#' @description Internal support function to run a list of reportings in
#' a \code{\link{tryReport}} environment.
#'
#' @param ... report function to be run
#' @param gdx gdx file to report from
#' @param level spatial level (either "regglo" for region+global or "iso" for ISO countries)
#' @return A list of magpie objects (successful reports) or NULL (failed reports)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{tryReport}}, \code{\link{reportCondition}}

tryList <- function(..., gdx, level = "regglo") {
  width <- max(nchar(c(...))) + 1

  # Control the number of cores, 3, as the standard number of
  # cores in a MAgPIE SLURM job is 3.
  withr::local_options(mc.cores = 3)

  conditions <- parallel::mclapply(
    unique(list(...)),
    tryReport,
    width, gdx, level = level, env = parent.frame(),
    # Do not preschedule, as jobs have very different durations
    # so cores should just take whatever comes next.
    mc.preschedule = FALSE
  )

  # Generate output table from condition objects
  for (cond in conditions) {
    message("   ", format(cond$report, width = width), cond$message, cond$elapsed)
  }

  return(lapply(conditions, function(cond) {
    cond$result
  }))
}
