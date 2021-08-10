#' @title reportGrasslandYields
#' @description reportGrasslandYields
#' 
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#' 
#' @param gdx GDX file
#' @return yield as MAgPIE object (Mt DM/ha)
#' @author Marcos Alves
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGrasslandYields(gdx)
#'   }
#' 

reportGrasslandYields <- function(gdx) {
  grass_yields <- NULL
  x <- NULL
  try({grass_yields<- readGDX(gdx, "ov_past_yld", format = "simplest")[, , list("type" = "level", "w" = "rainfed")]})
  if(!is.null(grass_yields)) {
    grass_yields <- collapseNames(grass_yields)
    x <- setNames(grass_yields, paste0("Productivity|Yield|+|", reportingnames(getNames(grass_yields)), "(t DM/ha)")) 
  } else {
    print("Disabled (No separate grassland yields)")
  }
  return(x)
} 
