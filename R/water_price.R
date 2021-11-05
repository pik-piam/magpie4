#' @title water_price
#' @description reads water prices from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level spatial level of aggregation: "cell" (cellular), "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param weight value sums regional weights by value of water per cluster, quantity sums regional weight by qty of water per cluster
#' @param index FALSE (default) or TRUE
#' @param index_baseyear baseyear to use for index calculation (only used if index=TRUE)
#' @param digits integer. For rounding of the return values
#' @return A MAgPIE object containing the water shadow prices (US Dollar/cubic metre).
#' @author Markus Bonsch, Vartika Singh, Miodrag Stevanovic
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- water_price(gdx)
#'   }
#' 

water_price <- function(gdx, file=NULL, level="reg", weight = "value", index=FALSE, index_baseyear=2005, digits=4) {
  
  #cellular level
  oq_water_cell <- readGDX(gdx,"oq43_water","oq_water", format="first_found")[,,"marginal"]
  oq_water_cell <- round(oq_water_cell,digits=digits)
  
  if(is.null(oq_water_cell)) {
    warning("Water shadow prices cannot be calculated as needed data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }
  
  ovm_watdem_cell <- setNames(readGDX(gdx,"ov_watdem","ov43_watdem","ovm_watdem", format="first_found")[,,"agriculture.level"],NULL)
  if(is.null(ovm_watdem_cell)) {
    warning("Water shadow prices cannot be calculated as needed data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }
  

  #cluster level water consumption
  weight_cell <- -oq_water_cell*ovm_watdem_cell
  
  weight_reg_qty <- superAggregate(ovm_watdem_cell,level=level,aggr_type="sum",crop_aggr=FALSE) #regional level weight based on qty of water
  weight_reg_value <- superAggregate(as.magpie(weight_cell),level=level,aggr_type="sum",crop_aggr=FALSE) #regional level weight based on value of water
  
   if(level=="cell"){
    water<- as.magpie(-oq_water_cell)
  } 
  
  if (level!="cell") {
    if (weight =="quantity") {
        water <- as.magpie(superAggregate(as.magpie(weight_cell),level=level,aggr_type="sum",crop_aggr=FALSE) / weight_reg_qty)
  } else if (weight == "value") {  
    water <- as.magpie(superAggregate(as.magpie(-oq_water_cell*weight_cell),level=level,aggr_type="sum",crop_aggr=FALSE) / weight_reg_value)
    }
  }
  
  
  if (index) {
    # check if the baseyear is contained in the gdx  
    if(!index_baseyear %in% getYears(water)) {
      water <- time_interpolate(water, index_baseyear, integrate_interpolated_years=TRUE)
         }
    water <- water/setYears(water[,index_baseyear,],NULL)*100
  }
  water <- as.magpie(round(water,digits))
  out(water,file)
}


