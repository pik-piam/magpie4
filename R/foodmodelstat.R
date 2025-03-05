#' @title foodmodelstat
#' @description MAgPIE food model statistics with information about convergence and number of iterations
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @return A MAgPIE object containing number of iterations and convergence information for each time step
#' @author Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' x <- foodmodelstat(gdx)
#' }
#'
#' @export
foodmodelstat <- function(gdx, file = NULL) {
  .read <- function(gdx, name, x, y) {
    yearsToBeChecked <- which(getYears(x, as.integer = TRUE) > as.integer(readGDX(gdx, "sm_fix_SSP2")))
    if (!is.null(x)) {
      getNames(x) <- paste0(name, " (limit = ", sub(pattern = "[.]", replacement = ",", as.character(y)), ")")
      if (!is.null(y) && any(x[, yearsToBeChecked, ] > y, na.rm = TRUE)) {
        warning(name, " limit violated in food model!", call. = FALSE)
      }
    }
    return(x)
  }

  conv <- .read(gdx, "convergence", lastIter(gdx, "p15_convergence_measure", MagpieOutput = FALSE), readGDX(gdx, "s15_convergence"))
  iter <- .read(gdx, "iterations", readGDX(gdx, "p15_iteration_counter"), readGDX(gdx, "s15_maxiter"))

  x <- mbind(conv, iter)
  out(x, file)
}
