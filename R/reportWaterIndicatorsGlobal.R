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

  indicatorname <- "Water|Share of population in water stressed region"
  unit          <- "share"
  # Def.: share of global population living in water stressed region
  watStress <- waterStress(gdx, stressRatio = 0.4, level = "cell")
  pop       <- population(gdx, level = "cell")

  out <- dimSums(pop * watStress, dim = 1) / dimSums(pop, dim = 1)

  getNames(out) <- paste0(indicatorname, " (", unit, ")")
  x             <- mbind(x, out)

  # return all indicators
  x <- x[, , sort(getNames(x))]

  return(x)

}
