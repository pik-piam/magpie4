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
#' @section Wage development variables:
#' Name | Unit | Meta
#' ---|---|---
#' Labor\|Wages\|Hourly labor costs relative to 2000 | index | Hourly labor cost development relative to base year
#' @md

#'
reportWageDevelopment <- function(gdx, baseYear = 2000, level = "regglo") {

  out <- wageDevelopment(gdx, baseYear = baseYear, level = level)

  if (!is.null(out)) {
    getNames(out) <- paste0("Labor|Wages|Hourly labor costs relative to ", baseYear, " (index)")
  }

  return(out)
}
