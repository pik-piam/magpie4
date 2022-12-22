#' @title laborProductivity
#' @description calculates labor productivity in crop sector (kg DM per hour)
#'              from a MAgPIE gdx file
#' @return labor productivity in crop sector (kg DM per hour)
#' @param gdx GDX file
#' @param level spatial aggregation to report productivity ("cell","reg", "regglo", "glo")
#' @param productAggr Aggregate over products or not (boolean)
#' @author Xiaoxi Wang, Ruiying Du, Debbora Leip
#' @importFrom magclass  collapseNames dimSums
#' @importFrom magpiesets findset
#' @importFrom gdx readGDX
#' @export
#' @examples
#' \dontrun{
#' x <- laborProductivity(gdx)
#' }

laborProductivity <- function(gdx, level = "reg", productAggr = TRUE) {

  laborHoursCell <- readGDX(gdx, "ov38_laborhours_need", format = "first_found", select = list(type = "level"))

  # for other facotr cost relizations then sticky_labor no labor hours are reported
  if (is.null(laborHoursCell)) return(NULL)

  # for sticky_labor realization, we can calculate productivities in terms of output per hour
  if (isTRUE(productAggr)) {
    x <- production(gdx, level = "cell", products = "kcr", product_aggr = FALSE)
    weight <- x / dimSums(x, dim = 3)
    weight[is.na(weight)] <- 0
    laborHoursCell <- dimSums(weight * laborHoursCell, dim = 3)
  }

  if (level == "cell") {
    productivity <- 1000 / laborHoursCell # kg DM per hour
  } else if (level %in% c("reg", "regglo", "glo")) {
    x <- production(gdx, level = "cell", products = "kcr", product_aggr = productAggr)
    laborHoursReg <- superAggregateX(laborHoursCell, level = level, aggr_type = "weighted_mean", weight = x)
    productivity <- 1000 / laborHoursReg # kg DM per hour
  } else {
    stop("An appropriate level is required!")
  }

  return(productivity)
 }
