#' @title reportProductionGrowth
#' @description reports production growth rate
#' 
#' @import magpiesets
#' @export
#' 
#' @param gdx GDX file
#' @param detail if true, provides results for all commodities, otherwhise aggregates some groups
#' @return Production growth rates (index)
#' @author Xiaoxi Wang
#' @examples
#' 
#'   \dontrun{
#'     x <- reportProductionGrowth(gdx="fulldata.gdx",detail=TRUE)
#'   }
#' 
reportProductionGrowth <- function(gdx,detail = FALSE){
  x <- NULL
  
  out <- production(gdx = gdx,level="regglo",products = readGDX(gdx,"kall"),product_aggr = FALSE,water_aggr = TRUE)
  out <- round(out,1)/setYears(round(out[,1,],1),NULL)
  out[is.nan(out)] <- 0
  out[is.infinite(out)] <- 0
  out<-reporthelper(x = out,dim=3.1,level_zero_name = "Production|Production Growth Rate ",detail = detail)
  getNames(out) <- paste(getNames(out),"(Index)",sep=" ")
  x <- mbind(x,out)
  x <- summationhelper(x)
  return(x)
}