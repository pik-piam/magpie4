#' @title reportGridLand
#' @description reports land-use from gridded (disaggregated) output
#' 
#' @export
#' 
#' @param gdx GDX file
#' 
#' @return land-use as MAgPIE object (million ha)
#' @author Jannes Breier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGridLand(gdx)
#'   }
#' 

reportGridLand <- function(gdx) {
  
    x <- land(gdx,level="grid",types = NULL,sum = FALSE)

  getNames(x) <- magpiesets::reportingnames(getNames(x))
  x <- metadata_comments(x=x,unit="Mha", description="Total land area in its primary land cover categories. Other includes non-forest natural vegetation like savannas.",comment="",note="")

  return(x)
}

