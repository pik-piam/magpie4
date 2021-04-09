#' @title CostsLabor
#' @description reads costs entering the objective function from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation ("reg", "glo", "regglo")
#' @param set Set for which to report production costs ("kcr", "kli", ...)
#' @return MAgPIE object containing labor costs [million US$05]
#' @author Debbora Leip
#' @importFrom gdx readGDX out
#' @importFrom luscale superAggregate
#' @importFrom magpiesets findset
#' @examples
#'
#'   \dontrun{
#'     x <- CostsLabor(gdx)
#'   }
#'

CostsLabor <- function(gdx, file = NULL, level = "reg", set = "kcr"){
  
  labor_costs <- readGDX(gdx,"ov_cost_prod", react = "silent", format = "first_found", select = list(type = "level"))[,,findset("kcr")]
  labor_costs <- superAggregate(labor_costs, aggr_type = "sum", level = level)
  
  out(labor_costs, file)
}