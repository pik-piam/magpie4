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
#' @return CO2 emissions as MAgPIE object (unit depends on \code{unit})
#' @author Florian Humpenoeder
#' @importFrom magclass new.magpie getCells lowpass
#' @examples
#' 
#'   \dontrun{
#'     x <- emisCO2(gdx)
#'   }
#' 

emisCO2 <- function(gdx, file=NULL, level="cell", unit="element", cumulative=FALSE, baseyear=1995, lowpass=NULL,cc=TRUE){
  
  #get carbon stocks
  stock <- carbonstock(gdx,sum=TRUE,level="cell",cc=cc)

  timestep_length <- readGDX(gdx,"im_years",react="silent")
  if(is.null(timestep_length)) timestep_length <- timePeriods(gdx)
  
  #calc emissions
  a <- new.magpie(getCells(stock),getYears(stock),NULL,NA)
  for (t in 2:length(timestep_length)) {
    if(cumulative) {
      if(baseyear == 1995) a[,1995,] <- 0
      a[,t,] <- (setYears(stock[,baseyear,],NULL) - stock[,t,])
    } else {
      a[,t,] <- (setYears(stock[,t-1,],NULL) - stock[,t,])/timestep_length[t]
    }
  }
  
  #unit conversion
  if (unit == "gas") a <- a*44/12 #from Mt C/yr to Mt CO2/yr

  #apply lowpass filter
  if(!is.null(lowpass)) a <- mbind(a[,1,],lowpass(a[,-1,],i=lowpass))

  #aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)

  out(a,file)
}
