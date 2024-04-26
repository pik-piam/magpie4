#' @title reportGridLand
#' @description reports land-use from gridded (disaggregated) output
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' 
#' @return land-use as MAgPIE object (million ha)
#' @author Jannes Breier
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGridLand(gdx)
#'   }
#' 

reportGridLand <- function(gdx,dir=".",spamfiledirectory="") {
  
  dir <- getDirectory(dir,spamfiledirectory)
  
  x <- land(gdx,level = "grid",types = NULL,sum = FALSE,dir=dir)

  getNames(x) <- magpiesets::reportingnames(getNames(x))
  x <- metadata_comments(x=x,unit="million ha/yr", description="Total land area in its primary land cover categories. Other includes non-forest natural vegetation like savannas.",comment="",note="")

  return(x)
}

