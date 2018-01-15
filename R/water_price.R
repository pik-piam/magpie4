#' @title water_price
#' @description reads water prices from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level spatial level of aggregation: "cell" (cellular), "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param index FALSE (default) or TRUE
#' @param index_baseyear baseyear to use for index calculation (only used if index=TRUE)
#' @param digits integer. For rounding of the return values
#' @return A MAgPIE object containing the water shadow prices (US Dollar/cubic metre).
#' @author Markus Bonsch
#' @examples
#' 
#'   \dontrun{
#'     x <- water_price(gdx)
#'   }
#' 

water_price <- function(gdx, file=NULL, level="reg", index=FALSE, index_baseyear=2005, digits=4) {
  oq_water_cell <- readGDX(gdx,"oq43_water","oq_water", format="first_found")[,,"marginal"]
  if(is.null(oq_water_cell)) {
    warning("Water shadow prices cannot be calculated as needed data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }

  if(level=="cell"){
    water<- as.magpie(-oq_water_cell)
  } else{
    ovm_watdem_cell <- setNames(readGDX(gdx,"ov_watdem","ov43_watdem","ovm_watdem", format="first_found")[,,"agriculture.level"],NULL)
    if(is.null(ovm_watdem_cell) ) {
      warning("Water shadow prices cannot be calculated as needed data could not be found in GDX file! NULL is returned!")
      return(NULL)
    }
    water <- as.magpie(superAggregate(as.magpie(-oq_water_cell*ovm_watdem_cell),level=level,aggr_type="sum",crop_aggr=FALSE) / superAggregate(ovm_watdem_cell,level=level,aggr_type="sum",crop_aggr=FALSE))
  }
  if (index) {
    water <- water/setYears(water[,index_baseyear,],NULL)*100
  }
  water <- as.magpie(round(water,digits))
  out(water,file)
}