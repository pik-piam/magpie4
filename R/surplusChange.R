#' @title surplusChange
#' @description calculates aggregate change in economic surplus in mio.US$ based on a MAgPIE gdx files from two different scenarios.
#' 
#' @export
#' 
#' @param gdx1 GDX file from benchmark scenario
#' @param gdx2 GDX file from the analyzed scenario
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param type Economic surplus type: "consumer" (default), "producer" or "welfare"
#' @return A MAgPIE object containing producers profit in million $US. 
#' @author Miodrag Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- surplusChange(gdx1, gdx2)
#'   }
#' 
#' @importFrom magclass as.magpie

surplusChange <- function(gdx1, gdx2, file=NULL, level="reg", type="consumer"){
  
  if(type=="consumer"){
    out <- consumptionValue(gdx1, level=level) - consumptionValue(gdx2, level=level)
  } else if(type=="producer"){
    out <- productionProfit(gdx2, level=level) - productionProfit(gdx1, level=level)
  } else if(type=="welfare"){
    out <- surplusChange(gdx1, gdx2, level=level, type="consumer") + 
           surplusChange(gdx1, gdx2, level=level, type="producer") 
  }
  
  out(out,file)
}