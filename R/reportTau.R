#' @title reportTau
#' @description reports Tau
#'
#' @param gdx GDX file
#' @return tau values as MAgPIE object (Index)
#' @author Florian Humpenoeder, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' x <- reportTau(gdx)
#' }
#'
#' @export
reportTau <- function(gdx) {
  pt <- NULL
  tau <- readGDX(gdx, "ov_tau", format = "first_found")[, , "level"]
  grassArea <- readGDX(gdx, "ov31_grass_area", format = "first_found", react = "silent")[, , "pastr.level"]
  if (any(grepl("pastr", getItems(tau, dim = 3))) && !is.null(grassArea)) {
    pt <- tau(gdx = gdx, level = "regglo", type = "pastr")
    if (!is.null(pt)) {
      getNames(pt) <- "Productivity|Landuse Intensity Indicator Tau managed pastures (Index)"
    }
  }
  cr <- tau(gdx = gdx, level = "regglo", type = "crop")
  getNames(cr) <- "Productivity|Landuse Intensity Indicator Tau (Index)"
  out <- mbind(cr, pt)
  return(out)
}
