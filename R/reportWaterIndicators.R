#' @title       reportWaterIndicators
#' @description reports a set of water indicators
#'
#' @export
#'
#' @param gdx       GDX file
#' @param level     level of aggregation (cluster: "cell", gridcells: "grid",
#'                  regional: "regglo", "reg", "glo")
#' @param outputdir output directory
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
#'
reportWaterIndicators <- function(gdx, level = "cell", outputdir = ".") {

  x <- NULL

  # @BENNI: Check aggregation

  indicatorname <- "Water|Environmental flow violation volume"
  unit          <- "km^3"
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


  indicatorname <- "Water|Water stress (total)"
  unit          <- "fraction"
  ## Def.: total quantity of freshwater withdrawal (agriculture, manufacturing, domestic; km^3) in the growing period
  ##       as a proportion of total available freshwater resources (km^3) in the growing period

  out <- waterStressRatio(gdx, level = level)

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)

  # return all indicators
  x <- x[, , sort(getNames(x))]

  return(x)

}
