#' @title reportCroparea
#' @description reports croparea
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return Croparea as MAgPIE object (million Ha/yr)
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportCroparea(gdx)
#' }
#'
reportCroparea <- function(gdx, detail = FALSE) {

  x <- NULL

  out <- croparea(gdx, level = "regglo", products = "kcr",
                  product_aggr = FALSE, water_aggr = TRUE)
  fallowland <- fallow(gdx, level = "regglo")
  treecover <- croplandTreeCover(gdx, level = "regglo")
  # Adapting reportingnames for fallow land and tree cover
  ReportingnameCroparea <- "Resources|Land Cover|Cropland|Croparea"
  x <- setNames(dimSums(out), "Resources|Land Cover|Cropland|+|Croparea (million ha)")

  out <- reporthelper(x = out, dim = 3.1,
                      level_zero_name = ReportingnameCroparea, detail = detail)
  getNames(out) <- paste(gsub("\\.", "|", getNames(out)), "(million ha)", sep = " ")
  out <- summationhelper(out, sep = "+")
  getNames(fallowland) <- paste0(
    "Resources|Land Cover|Cropland|+|",
    reportingnames(getNames(fallowland)), " (million ha)")
  getNames(treecover) <- paste0(
    "Resources|Land Cover|Cropland|+|",
    reportingnames(getNames(treecover)), " (million ha)")
  x <- mbind(x, fallowland, treecover, out)

  out <- croparea(gdx, level = "regglo", products = "kcr",
                  product_aggr = FALSE, water_aggr = FALSE)
  total <- dimSums(out, dim = 3.1)
  getNames(total) <- paste(ReportingnameCroparea, "|", reportingnames(getNames(total)),
                           " (million ha)", sep = "")
  out <- reporthelper(x = out, dim = 3.1,
                      level_zero_name = ReportingnameCroparea, detail = detail)
  getNames(out) <- paste(gsub("\\.", "|", getNames(out)), "(million ha)", sep = " ")
  out <- mbind(out, total)
  out <- summationhelper(out, sep = "++")

  x <- mbind(x, out)

  return(x)
}
