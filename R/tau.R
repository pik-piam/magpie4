#' @title tau
#' @description Calculates Landuse intensity indicator tau based on a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param start_value If TRUE, the initial values are added under the year \code{prev_year}
#' @param digits The result will be rounded to this number of digits
#' @param prev_year Year to store the initialization tau information in
#' @param type type of tc 'pastr' or 'crop'; or "both" if both are needed
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
        cr <- croparea(gdx, level = tauLevel, water_aggr = TRUE)
        if (is.null(cr)) {
          warning("tau cannot be aggregated as croparea function returned NULL! NULL is returned!")
          return(NULL)
        }
        if (start_value) {
          cr <- mbind(setYears(cr[, "y1995", ], prev_year), cr)
        }
        x <- superAggregate(x, aggr_type = "weighted_mean", level = level, weight = cr)
      }
    }

    ### if only "pastr" Tau is desired
    if (type == "pastr") {
      x <- x[, , "pastr.level"]
      getNames(x) <- NULL
      if (is.null(x)) {
        warning("No Information on tau in the gdx file! NULL is returned!")
        return(NULL)
      }
      if (start_value) {
        tau1995 <- readGDX(gdx, "f13_pastr_tau_hist", "fm_pastr_tau_hist", format = "first_found")[, 1995, ]
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
        pt <- NULL
        pt <- readGDX(gdx, "ov31_grass_area", format = "first_found", react = "silent")[, , "pastr.level"]
        if (is.null(pt)) {
          warning("Grassland areas not disaggregated. Tau for managed pastures cannot be calculated. NULL returned")
          return(NULL)
        }
        if (start_value) {
          pt <- mbind(setYears(pt[, "y1995", ], prev_year), pt)
        }
        x <- superAggregate(x, aggr_type = "weighted_mean", level = level, weight = pt)
      }
    }

    ### For both "crop" and "pastr" Tau (running exo Tau with "grasslands_apr22" realiz of `31_past` module)
    if (type == "both") {
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
        cr <- croparea(gdx, level = tauLevel, water_aggr = TRUE)
        if (is.null(cr)) {
          warning("tau cannot be aggregated as croparea function returned NULL! NULL is returned!")
          return(NULL)
        }
        if (start_value) {
          cr <- mbind(setYears(cr[, "y1995", ], prev_year), cr)
        }
        x <- superAggregate(x, aggr_type = "weighted_mean", level = level, weight = cr)
      }
      x <- collapseNames(x) # Drop `.level` from dim 3 names
    }

    # account for default realization of `31_past` module with only "crop" Tau (no "pastr" Tau) where dim(x)[3] == 1
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
      cr <- croparea(gdx, level = tauLevel, water_aggr = TRUE)
      if (is.null(cr)) {
        warning("tau cannot be aggregated as croparea function returned NULL! NULL is returned!")
        return(NULL)
      }
      if (start_value) {
        cr <- mbind(setYears(cr[, "y1995", ], prev_year), cr)
      }
      x <- superAggregate(x, aggr_type = "weighted_mean", level = level, weight = cr)
    }
  }

  out(round(x, digits), file)
}
