#' @title outputCheck
#' @description Function to check a MAgPIE gdx file for known problems
#' (e.g. non-zero dummy variables). The function will throw warnings for
#' problem found in the outputs.
#'
#' @param gdx GDX file
#' @author Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' outputCheck(gdx)
#' }
#' @export
#'

outputCheck <- function(gdx) {
  .addS <- function(x) {
    return(ifelse(length(x) > 1, "s", ""))
  }

  .reportWarnings <- function(...) {
    w <- c(...)
    if (length(w) == 0) {
      message("Output looks good, no problems found!")
    } else {
      for (wi in w) {
        warning(wi, call. = FALSE)
        message("## WARNING: ", wi)
      }
      message("## Please check your run, ", length(w), " problem", .addS(w), " identified!")
    }
  }

  .checkExists <- function(gdx) {
    if (file.exists(gdx)) return(NULL)
    return("fulldata.gdx not found!")
  }

  .checkTradeManna <- function(gdx) {
    x <- readGDX(gdx, "v21_manna_from_heaven", react = "silent")
    if (is.null(x) || all(x == 0)) return(NULL)
    return("v21_manna_from_heaven contains non-zero values!")
  }

  .checkFoodModelConvergence <- function(gdx) {
    iter <- readGDX(gdx, "p15_iteration_counter", react = "silent")
    iterMax <- readGDX(gdx, "s15_maxiter", react = "silent")
    if (is.null(iter) || is.null(iterMax) || all(iter <= iterMax)) return(NULL)
    violatingYears <- getYears(iter, as.integer = TRUE)[iter > iterMax]
    return(paste0("The food demand model did not converge in ",
                  length(violatingYears), " timestep", .addS(violatingYears), " (",
                  paste(violatingYears, collapse = ", "), ")!"))
  }

  w <- .checkExists(gdx)
  if (is.null(w)) {
    w <- c(.checkFoodModelConvergence(gdx),
           .checkTradeManna(gdx))
  }
  .reportWarnings(w)
}
