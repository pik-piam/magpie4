#' @title tau
#' @description Calculates Landuse intensity indicator tau based on a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregateX
#' @param start_value If TRUE, the initial values are added under the year \code{prev_year}
#' @param digits The result will be rounded to this number of digits
#' @param prev_year Year to store the initialization tau information in
#' @param type currently only "crop"
#' @return A MAgPIE object containing tau values (index)
#' @author Jan Philipp Dietrich, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- tau(gdx)
#' }
#'
tau <- function(gdx, file = NULL, level = "reg", start_value = FALSE, digits = 4, prev_year = "y1985", type = "crop") { # nolint

  x <- readGDX(gdx, "ov_tau", format = "first_found")[, , "level"]
  if (getSets(x)[1] == "j") {
    tauLevel <- "cell"
  } else {
    tauLevel <- "reg"
  }
  if (dim(x)[3] > 1) {
    ### If only "crop" Tau is desired
    if (type == "crop") {
      x <- x[, , "crop.level"]
      getNames(x) <- NULL
      if (is.null(x)) {
        warning("No Information on crop tau in the gdx file! NULL is returned!")
        return(NULL)
      }
      if (start_value) {
        tau1995 <- readGDX(gdx, "f_tau1995", "fm_tau1995", format = "first_found")
        if (is.null(x)) {
          warning("No Information on initial value for tau found in the gdx file! NULL is returned!")
          return(NULL)
        }
        if (tauLevel == "cell") {
          cell <- readGDX(gdx, "cell", react = "silent")
          tau1995 <- toolAggregate(tau1995, cell, from = "i", to = "j")
        }
        x <- mbind(setYears(tau1995, prev_year), x)
      }

      # bring superregional data back to regional level, if necessary
      supreg <- readGDX(gdx, "supreg", react = "silent")
      if (!is.null(supreg) && any(supreg$h != supreg$i)) {
        x <- toolAggregate(x, supreg)
      }

      if (level != tauLevel) {
        cropArea <- croparea(gdx, level = tauLevel, water_aggr = TRUE)
        if (is.null(cropArea)) {
          warning("tau cannot be aggregated as croparea function returned NULL! NULL is returned!")
          return(NULL)
        }
        if (start_value) {
          cropArea <- mbind(setYears(cropArea[, "y1995", ], prev_year), cropArea)
        }
        x <- superAggregateX(x, aggr_type = "weighted_mean", level = level, weight = cropArea)
      }
    }
  } else {
    getNames(x) <- NULL
    if (is.null(x)) {
      warning("No Information on tau in the gdx file! NULL is returned!")
      return(NULL)
    }
    if (start_value) {
      tau1995 <- readGDX(gdx, "f_tau1995", "fm_tau1995", format = "first_found")
      if (is.null(x)) {
        warning("No Information on initial value for tau found in the gdx file! NULL is returned!")
        return(NULL)
      }
      if (tauLevel == "cell") {
        cell <- readGDX(gdx, "cell", react = "silent")
        tau1995 <- toolAggregate(tau1995, cell, from = "i", to = "j")
      }
      x <- mbind(setYears(tau1995, prev_year), x)
    }

    # bring superregional data back to regional level, if necessary
    supreg <- readGDX(gdx, "supreg", react = "silent")
    if (!is.null(supreg) && any(supreg$h != supreg$i)) {
      x <- toolAggregate(x, supreg)
    }

    if (level != tauLevel) {
      cropArea <- croparea(gdx, level = tauLevel, water_aggr = TRUE)
      if (is.null(cropArea)) {
        warning("tau cannot be aggregated as croparea function returned NULL! NULL is returned!")
        return(NULL)
      }
      if (start_value) {
        cropArea <- mbind(setYears(cropArea[, "y1995", ], prev_year), cropArea)
      }
      x <- superAggregateX(x, aggr_type = "weighted_mean", level = level, weight = cropArea)
    }
  }

  out(round(x, digits), file)
}
