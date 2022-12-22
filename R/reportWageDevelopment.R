#' @title reportWageDevelopment
#' @description reports indicator on wage development: hourly labor costs in each time step relative to hourly labor
#' costs in 2000
#'
#' @export
#'
#' @param gdx GDX file
#' @param baseYear year relative to which the wage development should be calculated
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @return  indicator on wage development as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' x <- reportWageDevelopment(gdx)
#' }
#'
reportWageDevelopment <- function(gdx, baseYear = 2000, level = "regglo") {

  out <- wageDevelopment(gdx, baseYear = baseYear, level = level)

  if (!is.null(out)) {
    getNames(out) <- paste0("Hourly labor costs relative to ", baseYear, " (unitless)")
  }

  return(out)
}
