#' @title reportCropareaGrid
#' @description reports croparea
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx GDX file
#' @return Croparea as MAgPIE object (million ha/yr)
#' @author Benjamin Bodirsky
#' @examples
#' \dontrun{
#' x <- reportCropareaGrid(gdx)
#' }
#'
#' @section Grid-level croparea:
#' This function produces grid-level (0.5 degree) croparea data for individual crops.
#' Variable names follow the reportingnames mapping (e.g., Cereals, Oilcrops, etc.).
#' @md

#'
reportCropareaGrid <- function(gdx) {

  a <- croparea(gdx, level = "grid", products = "kcr", product_aggr = FALSE, water_aggr = TRUE)

  # no renaming for grid
  x <- setNames(a, reportingnames(getNames(a, dim = 1)))
  x <- metadata_comments(x = x, unit = "million ha/yr", description = "Croparea in physical area",
    comment = "", note = "")
  return(x)
}
