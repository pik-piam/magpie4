#' @title secdforestNaturalOrigin
#' @description Secondary forest area that grew on abandoned land, i.e. matured from young secondary
#' forest, per age class. Taken from the gdx where the model tracked it
#' (p35_secdforest_natural), otherwise reconstructed from the module 35 area bookkeeping:
#' it ages with the age classes, loses its share of disturbance and of the reduction in the solve,
#' and gains the matured young secondary forest. Secondary forest of the first time step counts as
#' other origin.
#'
#' @export
#'
#' @param gdx GDX file
#' @param reconstruct reconstruct even if the gdx contains p35_secdforest_natural
#' @param protect take reductions in the solve from other-origin area first (as in model versions that
#' kept natural-origin area from harvest) instead of proportionally
#' @return MAgPIE object (cell, year, ac) in Mha; NULL if the gdx lacks the module 35 parameters
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- secdforestNaturalOrigin(gdx)
#' }
secdforestNaturalOrigin <- function(gdx, reconstruct = FALSE, protect = FALSE) {
  if (!reconstruct) {
    natural <- readGDX(gdx, "p35_secdforest_natural", react = "silent")
    if (!is.null(natural)) {
      return(natural)
    }
  }
  areaPostSolve <- readGDX(gdx, "ov35_secdforest", select = list(type = "level"), react = "silent")
  areaPreSolve  <- readGDX(gdx, "p35_secdforest", react = "silent")
  matured       <- readGDX(gdx, "p35_maturesecdf", react = "silent")
  disturbance   <- readGDX(gdx, "p35_disturbance_loss_secdf", react = "silent")
  if (is.null(areaPostSolve) || is.null(areaPreSolve) || is.null(matured) || is.null(disturbance)) {
    return(NULL)
  }

  years <- getYears(areaPostSolve, as.integer = TRUE)
  ageClasses <- getItems(areaPostSolve, dim = 3)
  nAgeClasses <- length(ageClasses)
  .toArray <- function(x) as.array(x[, getYears(areaPostSolve), ageClasses])
  postSolve <- .toArray(areaPostSolve)
  preSolve <- .toArray(areaPreSolve)
  maturedArea <- .toArray(matured)
  disturbedArea <- .toArray(disturbance)

  naturalArea <- postSolve * 0
  naturalPrev <- postSolve[, 1, ] * 0
  for (t in seq_along(years)) {
    shift <- if (t == 1) 1 else (years[t] - years[t - 1]) / 5
    if (t > 1) {
      disturbedShare <- ifelse(postSolve[, t - 1, ] > 1e-10, disturbedArea[, t, ] / postSolve[, t - 1, ], 0)
      naturalPrev <- naturalPrev * (1 - pmin(disturbedShare, 1))
    }
    # age-class shift by `shift`, with accumulation in acx
    shifted <- naturalPrev * 0
    shifted[, (shift + 1):nAgeClasses] <- naturalPrev[, 1:(nAgeClasses - shift)]
    shifted[, nAgeClasses] <- shifted[, nAgeClasses] +
      rowSums(naturalPrev[, (nAgeClasses - shift + 1):nAgeClasses, drop = FALSE])
    current <- pmin(shifted + maturedArea[, t, ], preSolve[, t, ])
    if (protect) {
      naturalPrev <- pmin(current, postSolve[, t, ])
    } else {
      keptShare <- ifelse(preSolve[, t, ] > 1e-10, pmin(postSolve[, t, ] / preSolve[, t, ], 1), 0)
      naturalPrev <- current * keptShare
    }
    naturalArea[, t, ] <- naturalPrev
  }
  natural <- areaPostSolve
  natural[, , ] <- naturalArea
  return(natural)
}
