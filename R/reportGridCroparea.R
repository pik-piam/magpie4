#' @title reportGridCroparea
#' @description reports Croparea from gridded (disaggregated) output
#' 
#' @export
#' 
#' @param gdx GDX file
#' 
#' @return area of cropland as MAgPIE object (million ha)
#' @author Jannes Breier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGridCroparea(gdx)
#'   }
#' 

reportGridCroparea <- function(gdx) {
  
  x <- croparea(gdx, level = "grid", products = "kcr",product_aggr = FALSE, water_aggr = TRUE)
  
  getNames(x) <- magpiesets::reportingnames(getNames(x))
  x <- metadata_comments(x=x,unit="Mha physical area", description="Croparea by plant type and irrigation in physical area",comment="",note="")
  
  return(x)
}

