#' @title reportWageDevelopment
#' @description reports indicator on wage development: hourly labor costs in each time step relative to hourly labor
#' costs in 2020
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @return  indicator on wage development as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' x <- reportWageDevelopment(gdx)
#' }
#'
reportWageDevelopment <- function(gdx, level = "regglo") {

  out <- wageDevelopment(gdx, level = level)

  if (!is.null(out)) {
    getNames(out) <- paste0("Hourly labor costs relative to 2020")
  }

  return(out)
}
