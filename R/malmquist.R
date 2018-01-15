#' @title malmquist
#' @description calcluates malmquist index based on a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @return A MAgPIE object containing the malmquist index 
#' @author Xiaoxi Wang
#' @importFrom nonparaeff faremalm2
#' @importFrom reshape2 melt
#' @importFrom stats reshape
#' @examples
#' 
#'   \dontrun{
#'     x <- malmquist(gdx)
#'   }


malmquist <- function(gdx,file=NULL) {
  level <- "reg" #please add to arguments (we need "reg", "glo" and "regglo"; superAggreate can handle this; see other functions)
  production <- setNames(production(gdx,level=level,product_aggr = TRUE),"production")
  costs <- setNames(costs(gdx, level=level,sum=FALSE)[,,"Input Factors"],"factorcosts")
  cropland <- setNames(land(gdx,level=level,types="crop"),"cropland")
  water <- setNames(water_usage(gdx,level=level)[,,"agriculture",drop=TRUE],"water")
  x <- mbind(production,costs,cropland,water)
  d <- as.data.frame(x,rev=2)
  if(level=="reg") {
    d <- reshape(d,timevar="data1",idvar=c("t","i"),direction="wide")    
  } else {
    d <- reshape(d,timevar="data1",idvar=c("t","i","j"),direction="wide")
    d$i <- paste(d$i,d$j,sep="_")
    d$j <- NULL
  }
  d$i <- as.factor(d$i)
  d$t <- as.factor(d$t)
  years <- levels(d$t)
  d$t <- as.numeric(d$t)
  names(d) <- gsub(".value.","",names(d),fixed = TRUE)
  m <- faremalm2(dat = d, noutput = 1, id = "i", year = "t")
  m$t <- as.factor(m$t)
  levels(m$t) <- paste(years[-length(years)],years[-1],sep="-")
  m <- suppressMessages(melt(m))
  m <- (as.magpie(m,tidy=TRUE,temporal=2))
  out(m,file)
}