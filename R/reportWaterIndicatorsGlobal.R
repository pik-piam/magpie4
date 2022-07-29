#' @title       reportWaterIndicatorsGlobal
#' @description reports a set of water indicators at global level
#'
#' @export
#'
#' @param gdx       GDX file
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
#' x <- reportWaterIndicatorsGlobal(gdx)
#' }
#'
reportWaterIndicatorsGlobal <- function(gdx, outputdir = ".") {

  x <- NULL

  indicatorname <- "Water|Global Environmental Flow Violation volume"
  unit          <- "km^3"
  # Def.: environmental flow violation volume

  violations <- waterEFViolation(gdx, level = "cell", digits = 4)

  out <- dimSums(violations, dim = 1)

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)


  indicatorname <- "Water|Share of total Irrigated Area suffering from Environmental Flow Violations"
  unit          <- "share"
  # Def.: irrigated areas that fall into a cluster where environmental flows are violated

  irrigArea  <- croparea(gdx, level = "cell",
                        product_aggr = TRUE, water_aggr = FALSE)[, , "irrigated"]
  violations <- waterEFViolation(gdx, level = "cell", digits = 4)
  violations[violations > 0] <- 1

  out <- dimSums(irrigArea * violations, dim = 1) / dimSums(irrigArea, dim = 1)

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)


  indicatorname <- "Water|Share of total Irrigated Area suffering from Environmental Flow Violations based on violation share"
  unit          <- "share"
  # Def.: irrigated areas that fall into environmental flows violation area based on share of EFV to available water

  watAvl     <- water_avail(gdx, level = "cell", sum = TRUE, digits = 15)
  efvVolume  <- waterEFViolation(gdx, level = "cell", digits = 4)
  shareEFV   <- ifelse(watAvl > 0, efvVolume / watAvl, 0)

  out <- dimSums(irrigArea * shareEFV, dim = 1) / dimSums(irrigArea, dim = 1)

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)


  indicatorname <- "Water|People under water stress"
  unit          <- "million"
  # Def.: number of people living in water stressed region
  # watStress <- waterStress(gdx, stressRatio = 0.4, level = "cell")
  # pop       <- population(gdx, level = "cell")

  # out <- dimSums(pop * watStress, dim = 1)

  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x             <- mbind(x, out)


  indicatorname <- "Water|Share of population in water stressed region"
  unit          <- "share"
  # Def.: share of global population living in water stressed region
  # watStress <- waterStress(gdx, stressRatio = 0.4, level = "cell")
  # pop       <- population(gdx, level = "cell")
  ### NOTE: aggregation from iso to cell not working yet. Figure out first and then
  ### update this function
  # out <- dimSums(pop * watStress, dim = 1) / dimSums(pop, dim = 1)

  # getNames(out) <- paste0(indicatorname, " (",unit,")")
  # x             <- mbind(x, out)

  # return all indicators
  x <- x[, , sort(getNames(x))]

  return(x)

}
