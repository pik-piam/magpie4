#' @title Biodiversity Intactness Index
#' @description reads biodiversity intactness index out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global).
#' @details Calculates global and regional biodiversity intactness index
#' @return Biodiversity intactness index (unitless)
#' @author Patrick v. Jeetze, Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass dimSums 
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- BII(gdx)
#'   }

BII <- function(gdx, file=NULL, level="glo") {
  a <- readGDX(gdx,"ov44_biodiv",select = list(type="level"),react = "silent")
  if(!is.null(a)) {
  a <- dimSums(a,dim=3)
  rr <- readGDX(gdx,"f44_rr_layer") #includes range rarity layer
  area <- land(gdx,level="cell",sum = TRUE)
  reg <- superAggregate(a,level="reg",aggr_type = "sum")/superAggregate(area*rr,level="reg",aggr_type = "sum")
  glo <- superAggregate(a,level="glo",aggr_type = "sum")/superAggregate(area*rr,level="glo",aggr_type = "sum")
  if(level == "reg") {x <- reg}
  else if (level == "glo") {x <- glo}
  else if (level == "regglo") {x <- mbind(reg,glo)}
  } else x <- NULL
  out(x,file)
}


