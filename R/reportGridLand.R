#' @title reportGridLand
#' @description reports land-use from gridded (disaggregated) output
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' 
#' @return land-use as MAgPIE object (million ha)
#' @author Jannes Breier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGridLand(gdx)
#'   }
#' 

reportGridLand <- function(gdx,spamfiledirectory="") {
  
  x <- land(gdx,level = "grid",types = NULL,sum = FALSE,spamfiledirectory=spamfiledirectory)

  getNames(x) <- magpiesets::reportingnames(getNames(x))
  x <- metadata_comments(x=x,unit="million ha/yr", description="Total land area in its primary land cover categories. Other includes non-forest natural vegetation like savannas.",comment="",note="")

  return(x)
}

