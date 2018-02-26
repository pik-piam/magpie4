#' @title reportTradeGrowth
#' @description reports trade growth rate
#' 
#' @import magpiesets
#' @export
#' 
#' @param gdx GDX file
#' @param detail if true, provides results for all commodities, otherwhise aggregates some groups
#' @return Trade growth rates (index)
#' @author Xiaoxi Wang
#' @examples
#' 
#'   \dontrun{
#'     x <- reportTradeGrowth(gdx="fulldata.gdx",detail=TRUE)
#'   }
#' 

reportTradeGrowth <- function(gdx,detail = FALSE){
  x <- NULL
  
  out<-trade(gdx,level = "regglo",type = "imports") + trade(gdx,level = "regglo",type = "exports")
  out["GLO",,] <- dimSums(trade(gdx,level ="reg", type ="exports"),dim=1)
  out <- round(out,1)/setYears(round(out[,1,],1),NULL)
  out[is.nan(out)] <- 0
  out[is.infinite(out)] <- 0
  out <-reporthelper(x=out,dim = 3.1,level_zero_name = "Trade|Trade Growth Rate", detail = detail)
  getNames(out) <- paste(getNames(out),"(Index)",sep=" ")
  x <- mbind(x,out)
  
  return(x)
}