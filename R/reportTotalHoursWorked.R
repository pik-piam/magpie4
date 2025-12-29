#' @title reportTotalHoursWorked
#' @description reports total hours worked in crop+livestock production (and maccs) from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @return total hours worked as MAgPIE object
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportTotalHoursWorked(gdx)
#'   }
#'
#'
#' @section Total hours worked variables:
#' Name | Unit | Meta
#' ---|---|---
#' Labor\|Total Hours Worked | mio h | Total hours worked in crop and livestock production
#' @md


reportTotalHoursWorked <- function(gdx, level = "regglo") {

  out <- totalHoursWorked(gdx, level = level)

  if (!is.null(out)) {
    out <- setNames(out, paste0("Labor|Total Hours Worked (mio h)"))
  }

  return(out)
}
