#' @title laborProductivity
#' @description calculates labor productivity in crop sector (kg DM per hour)
#'              from a MAgPIE gdx file
#' @return labor productivity in crop sector (kg DM per hour)
#' @param gdx GDX file
#' @param level spatial aggregation to report productivity ("cell","reg")
#' @param product_aggr Aggregate over products or not (boolean)
#' @author Xiaoxi Wang, Ruiying Du
#' @importFrom magclass  collapseNames dimSums
#' @importFrom magpiesets findset
#' @importFrom gdx readGDX
#' @export
#' @examples
#' \dontrun{
#' x <- laborProductivity(gdx)
#' }


# set function
laborProductivity <- function(gdx, level = "reg", product_aggr = TRUE) {
  kcr <- findset("kcr")
  labor_hours_cell <- readGDX(gdx, c("ov38_labor_need","ov38_laborhours_need"),
                                format = "first_found",select = list(type="level"))[,,kcr]
  labor_hours_cell <- collapseNames(labor_hours_cell)
  if(level=="cell"){
    productivity <- 1000/labor_hours_cell # kg DM per hour
  } else if (level=="reg"){
    x <- production(gdx,level= "cell",products= "kcr", product_aggr= product_aggr)
    labor_hours_reg <- superAggregateX(labor_hours_cell,level=level,aggr_type="weighted_mean",weight=x)
    productivity <- 1000/labor_hours_reg # kg DM per hour
  }else {stop("An appropriate level is required!")}

  if(isTRUE(product_aggr)){
      x <- production(gdx,level= level,products= "kcr", product_aggr= FALSE)
      y <- dimSums(x,dim=3)
      weight <- x/y
      productivity <- weight*productivity
      productivity <- dimSums(productivity,dim=3)
  }
   return(productivity)
 }

