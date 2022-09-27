#' @title       reportWaterIndicators
#' @description reports a set of water indicators
#'
#' @export
#'
#' @param gdx       GDX file
#'
#' @return MAgPIE object
#'
#' @author Felicitas Beier
#'
#' @importFrom magclass getNames mbind dimSums
#'
#' @examples
#' \dontrun{
#' x <- reportWaterIndicators(gdx)
#' }

reportWaterIndicators <- function(gdx) {

  x     <- NULL
  level <- "regglo"

  indicatorname <- "Water|Environmental flow violation volume"
  unit          <- "km3"
  # Def.: volume of environmental flow violations

  efvVolume <- waterEFViolation(gdx, level = level, digits = 4)
  out       <- efvVolume

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)


  indicatorname <- "Water|Environmental flow violation share of total water withdrawals"
  unit          <- "share"
  # Def.: environmental flow violation share of total human water withdrawals

  wwHuman <- dimSums(water_usage(gdx, level = level, digits = 15,
                                 users = "sectors", sum = FALSE)[, , "ecosystem", invert = TRUE], dim = 3)
  out     <- ifelse(wwHuman > 0, efvVolume / wwHuman, 0)

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)


  indicatorname <- "Water|Environmental flow violation share of water availability"
  unit          <- "share"
  # Def.: environmental flow violation share of water availability in the growing period

  watAvl  <- water_avail(gdx, level = level, sum = TRUE, digits = 15)
  out     <- ifelse(watAvl > 0, efvVolume / watAvl, 0)

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)


  indicatorname <- "Water|Irrigated Area suffering under Environmental Flow Violation"
  unit          <- "Mha"
  # Def.: area that falls in cluster with EFV

  out <- waterEFVarea(gdx, level = level)

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)


  indicatorname <- "Water|Share of total Irrigated Area suffering from Environmental Flow Violations"
  unit          <- "share"
  # Def.: irrigated areas that fall into a cluster where environmental flows are violated

  irrigArea <- water_AAI(gdx, level = "cell")
  efvArea   <- waterEFVarea(gdx, level = "cell")

  out <- efvArea / irrigArea
  out <- gdxAggregate(gdx, x = out, to = level, absolute = FALSE,
                      weight = "water_AAI")

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)


  indicatorname <- "Water|Withdrawal to Availability ratio"
  unit          <- "fraction"
  ## Def.: total quantity of freshwater withdrawal (agriculture, manufacturing, domestic; km^3) in the growing period
  ##       as a proportion of total available freshwater resources (km^3) in the growing period

  out <- waterStressRatio(gdx, level = level)

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)

  return(x)

}
