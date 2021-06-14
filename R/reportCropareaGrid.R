#' @title reportCropareaGrid
#' @description reports croparea
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx GDX file
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @return Croparea as MAgPIE object (million ha/yr)
#' @author Benjamin Bodirsky
#' @examples
#' \dontrun{
#' x <- reportCropareaGrid(gdx)
#' }
#'
reportCropareaGrid <- function(gdx, dir = ".", spamfiledirectory = "") {

  dir <- getDirectory(dir, spamfiledirectory)
  a <- croparea(gdx, level = "grid", products = "kcr", product_aggr = FALSE, water_aggr = TRUE, dir = dir)

  # no renaming for grid
  x <- setNames(a, reportingnames(getNames(a, dim = 1)))
  x <- metadata_comments(x = x, unit = "million ha/yr", description = "Croparea in physical area",
    comment = "", note = "")
  return(x)
}
