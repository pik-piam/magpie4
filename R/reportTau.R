#' @title reportTau
#' @description reports Tau
#'
#' @export
#'
#' @param gdx GDX file
#' @return tau values as MAgPIE object (Index)
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' x <- reportTau(gdx)
#' }
#'
reportTau <- function(gdx) {
  pt = NULL
  tau <- readGDX(gdx, "ov_tau", format = "first_found")[, , "level"]
  if(any(grepl("pastr",getItems(tau, dim = 3)))) {
      pt <- tau(gdx = gdx, level = "regglo", type = "pastr")
      if(!is.null(pt)){
        getNames(pt) <- "Productivity|Landuse Intensity Indicator Tau managed pastures (Index)"  
      }
  }
  cr <- tau(gdx = gdx, level = "regglo", type = "crop")
  getNames(cr) <- "Productivity|Landuse Intensity Indicator Tau (Index)"
  out <- mbind(cr, pt)
  return(out)
}
"superAggregate"