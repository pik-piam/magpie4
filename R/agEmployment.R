#' @title agEmployment
#' @description returns employment in crop+livestock production from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param type "absolute" for total number of people employed, "share" for share out of working age population
#' @param level spatial aggregation to report employment ("reg", "glo" or "regglo")
#' @param file a file name the output should be written to using write.magpie
#' @return employment in agriculture as absolute value or as percentage of working age population
#' @author Debbora Leip
#' @importFrom luscale superAggregate
#' @examples
#' \dontrun{
#' x <- agEmployment(gdx)
#' }

agEmployment <- function(gdx, type = "absolute", level = "reg", file = NULL) {

  agEmpl <- readGDX(gdx, "ov36_employment", select = list(type = "level"), react = "silent")

  workingAge <- c("15--19", "20--24", "25--29", "30--34", "35--39", "40--44", "45--49", "50--54", "55--59", "60--64")
  population <- dimSums(population(gdx, level = level, age = TRUE)[, , workingAge], dim = 3)

  if (!is.null(agEmpl)) {
    x <- superAggregate(agEmpl, level = level, aggr_type = "sum")
    if (type == "share") {
      x <- (x / population) * 100
    }
  } else { # for MAgPIE versions before implementation of employment return NULL
    x <- NULL
  }

  out(x, file)
}
