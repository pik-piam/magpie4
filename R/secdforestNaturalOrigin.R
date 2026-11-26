#' @title secdforestNaturalOrigin
#' @description Secondary forest area that matured from young secondary forest on abandoned land
#' (natural succession), per age class. Taken from the gdx where the model tracked it
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
#' @importFrom magclass getYears getItems
#' @examples
#' \dontrun{
#' x <- secdforestNaturalOrigin(gdx)
#' }
secdforestNaturalOrigin <- function(gdx, reconstruct = FALSE, protect = FALSE) {
  if (!reconstruct) {
    natural <- readGDX(gdx, "p35_secdforest_natural", react = "silent")
    if (!is.null(natural)) return(natural)
  }
  post   <- readGDX(gdx, "ov35_secdforest", select = list(type = "level"), react = "silent")
  pre    <- readGDX(gdx, "p35_secdforest", react = "silent")
  mature <- readGDX(gdx, "p35_maturesecdf", react = "silent")
  loss   <- readGDX(gdx, "p35_disturbance_loss_secdf", react = "silent")
  if (is.null(post) || is.null(pre) || is.null(mature) || is.null(loss)) return(NULL)

  years <- getYears(post, as.integer = TRUE)
  ac <- getItems(post, dim = 3)
  .arr <- function(x) as.array(x[, getYears(post), ac])
  postA <- .arr(post)
  preA <- .arr(pre)
  matureA <- .arr(mature)
  lossA <- .arr(loss)
  nac <- length(ac)

  natural <- post
  natural[, , ] <- 0
  natA <- postA
  natA[, , ] <- 0
  prev <- postA[, 1, ] * 0
  for (t in seq_along(years)) {
    s <- if (t == 1) 1 else (years[t] - years[t - 1]) / 5
    if (t > 1) {
      lossShare <- ifelse(postA[, t - 1, ] > 1e-10, lossA[, t, ] / postA[, t - 1, ], 0)
      prev <- prev * (1 - pmin(lossShare, 1))
    }
    # age-class shift by s, with accumulation in acx
    shifted <- prev * 0
    shifted[, (s + 1):nac] <- prev[, 1:(nac - s)]
    shifted[, nac] <- shifted[, nac] + rowSums(prev[, (nac - s + 1):nac, drop = FALSE])
    current <- pmin(shifted + matureA[, t, ], preA[, t, ])
    if (protect) {
      prev <- pmin(current, postA[, t, ])
    } else {
      prev <- current * ifelse(preA[, t, ] > 1e-10, pmin(postA[, t, ] / preA[, t, ], 1), 0)
    }
    natA[, t, ] <- prev
  }
  natural[, , ] <- natA
  return(natural)
}
