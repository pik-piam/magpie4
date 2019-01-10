#' @title emisCO2
#' @description reads detailed CO2 emissions out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param unit "element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr
#' @param cumulative Logical; Determines if emissions are reported annually (FALSE) or cumulative (TRUE). The starting point for cumulative emissions is y1995.
#' @param baseyear Baseyear used for cumulative emissions (default = 1995)
#' @param lowpass number of lowpass filter iterations
#' @param cc account for climate change impacts on carbon stocks (default = TRUE). FALSE reflects only carbon stock changes due to land management.
#' @param type net emissions (net), positive emissions only (pos) or negative emissions only (neg). Default is "net", which is the sum of positive and negative emissions
#' @return CO2 emissions as MAgPIE object (unit depends on \code{unit})
#' @author Florian Humpenoeder
#' @importFrom magclass new.magpie getCells lowpass
#' @examples
#' 
#'   \dontrun{
#'     x <- emisCO2(gdx)
#'   }
#' 

emisCO2 <- function(gdx, file=NULL, level="cell", unit="element", cumulative=FALSE, baseyear=1995, lowpass=NULL, cc=TRUE, type="net"){
  
  #get carbon stocks
  stock <- carbonstock(gdx,level="cell",cc=cc)
 
  timestep_length <- readGDX(gdx,"im_years",react="silent")
  if(is.null(timestep_length)) timestep_length <- timePeriods(gdx)
  
  #calc emissions
  a <- new.magpie(getCells(stock),getYears(stock),NULL,NA)
  for (t in 2:length(timestep_length)) {
    a[,t,] <- (setYears(stock[,t-1,],NULL) - stock[,t,])/timestep_length[t]
  }
  
  #unit conversion
  if (unit == "gas") a <- a*44/12 #from Mt C/yr to Mt CO2/yr
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    carbon_wood <- collapseNames(dimSums(carbonHWP(gdx,level = level,unit = unit)[,,"wood"],dim=3.1))/timestep_length[t]
    carbon_woodfuel <- collapseNames(dimSums(carbonHWP(gdx,level = level,unit = unit)[,,"woodfuel"],dim=3.1))/timestep_length[t]
    # carbon_in_wood <- new.magpie(getCells(carbon_hwp),getYears(carbon_hwp),NULL,NA)
    # for (t in 2:length(timestep_length)) {
    #  carbon_in_wood[,t,] <- (setYears(carbon_hwp[,t-1,],NULL) - carbon_hwp[,t,])/timestep_length[t]
    # }
    # a <- a - carbon_in_wood
    a <- a - carbon_wood + carbon_woodfuel
  }
  
  #years
  years <- getYears(a,as.integer = T)
  yr_hist <- years[years <= 2010]
  yr_fut <- setdiff(years,yr_hist)

  #apply lowpass filter (not applied on 1st time step, applied seperatly on historic and future period)
  if(!is.null(lowpass)) a <- mbind(a[,1995,],lowpass(a[,yr_hist[yr_hist>1995],],i=lowpass),lowpass(a[,yr_fut,],i=lowpass))
  
  #net, pos or negative
  if (type == "net") {
    a <- a
  } else if (type == "pos") {
    a[a < 0] = 0
  } else if (type == "neg") {
    a[a > 0] = 0
  }
  
  if (cumulative) {
    im_years <- new.magpie("GLO",years,NULL)
    im_years[,,] <- c(1,diff(years))
    a[,"y1995",] <- 0
    a <- a*im_years[,getYears(a),]
    a <- as.magpie(apply(a,c(1,3),cumsum))
    a <- a - setYears(a[,baseyear,],NULL)
  }
  
  #aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)

  out(a,file)
}
