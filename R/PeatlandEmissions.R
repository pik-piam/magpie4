#' @title PeatlandEmissions
#' @description reads peatland GHG emissions out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any aggregation level defined in superAggregate. In addition "climate" for the 3 climate regions tropical, temperate and boreal is available.
#' @param unit global warming potential (gwp) or gas (gas)
#' @param cumulative FALSE (default) or TRUE
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @param lowpass number of lowpass filter iterations (default = 0)
#' @details Peatland GHG emissions: CO2, DOC, CH4 and N2O
#' @return Peatland GHG emissions in Mt CO2eq (if unit="gwp") or Mt of the respective gas (if unit="gas")
#' @author Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass dimSums collapseNames new.magpie getYears lowpass
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- PeatlandArea(gdx)
#'   }

PeatlandEmissions <- function(gdx, file=NULL, level="cell", unit="gwp", cumulative=FALSE, baseyear=1995, lowpass=0){
  
  a <- readGDX(gdx,"ov58_peatland_emis",select=list(type="level"),react = "silent")
  if(!is.null(a)) {
    if(level == "climate") {
      map_cell_clim <- readGDX(gdx,"p58_mapping_cell_climate")
      ov58_peatland_man <- readGDX(gdx,"ov58_peatland_man",select = list(type="level"))
      p58_ipcc_wetland_ef <- readGDX(gdx,"p58_ipcc_wetland_ef")
      a <- ov58_peatland_man*map_cell_clim*p58_ipcc_wetland_ef
      a <- dimSums(a,dim=c(1,3.1,3.2))
    } else if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
    
    if(unit=="gas") {
      #34 and 298 because Wilson et al (2016) used these GWP100 factors from AR5 for the conversion of wetland emission factors
      a[,,"ch4"] <- a[,,"ch4"]/34
      a[,,"n2o"] <- a[,,"n2o"]/298
    }
    
    #years
    years <- getYears(a,as.integer = T)
    yr_hist <- years[years > 1995 & years <= 2020]
    yr_fut <- years[years >= 2020]
    
    #apply lowpass filter (not applied on 1st time step, applied seperatly on historic and future period)
    if(!is.null(lowpass)) a <- mbind(a[,1995,],lowpass(a[,yr_hist,],i=lowpass),lowpass(a[,yr_fut,],i=lowpass)[,-1,])
    
    if (cumulative) {
      im_years <- new.magpie("GLO",getYears(a),NULL)
      im_years[,,] <- c(1,diff(getYears(a,as.integer = TRUE)))
      a[,"y1995",] <- 0
      a <- a*im_years[,getYears(a),]
      a <- as.magpie(apply(a,c(1,3),cumsum))
      a <- a - setYears(a[,baseyear,],NULL)
    }
  } else a <- NULL
  
  out(a,file)
}