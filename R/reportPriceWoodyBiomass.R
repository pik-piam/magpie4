#' @title reportPriceWoodyBiomass
#' @description reports woody biomass prices (land rent)
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return land prices as MAgPIE object Unit: see names
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportPriceWoodyBiomass(gdx)
#'   }
#' 

reportPriceWoodyBiomass<-function(gdx){
  timber <- FALSE
  fore_red <- readGDX(gdx,"ov32_land_reduction","ov_forestry_reduction",select = list(type="level"),react = "silent", format="first_found")
  if (!is.null(fore_red)) {
    if (max(fore_red) > 1) {
      if(readGDX(gdx,"s73_timber_demand_switch","sm_timber_demand_switch")){
        timber <- TRUE
      }
    }
  }
  if(timber) {
    x <- prices(gdx, level="regglo",attributes = "dm")[,,findset("kforestry")]
    getNames(x) <- reportingnames(getNames(x))
    getNames(x) <- paste0("Prices|", getNames(x) ," (US$17/tDM)")
  } else {
    x <- NULL
    message("Not reported for magpie runs without woody biomasss production.")
  }
  
  return(x)
}