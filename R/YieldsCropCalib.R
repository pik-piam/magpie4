#' @title YieldsCropCalib
#' @description Reads potential yields after calibration
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @return A MAgPIE object containing values of potential yields after the calibration routines
#' @author Edna Molina Bacca
#' @importFrom gdx readGDX out
#' @importFrom magpiesets findset
#' @importFrom magclass magpiesort
#' @importFrom luscale speed_aggregate
#' @examples
#' \dontrun{
#' x <- YieldsCropCalib(gdx)
#' }
#'
YieldsCropCalib <- function(gdx, file = NULL, level = "cell", dir = ".") {

  kcr <- findset("kcr")
  t<-readGDX(gdx, "t")
  out <- readGDX(gdx, "i14_yields_calib")[,t,kcr]
  
  weight <- out
  area<-area<-croparea(gdx,level="grid",products="kcr", product_aggr=FALSE, water_aggr=FALSE,dir=dir)[,1995,]
  grid_to_cell = retrieve_spamfile(gdx=gdx,dir=dir)
  area<-if(level=="grid") area else if(level %in% c("glo","reg","regglo")) magpiesort(speed_aggregate(area,from="grid",to="cell",weight=NULL,rel=grid_to_cell))
  weight[, , ]<- area
  
  if (level != "cell") out <- gdxAggregate(gdx, out, weight = weight, to = level, absolute = FALSE, dir = dir)
  

  out(out, file)
}
