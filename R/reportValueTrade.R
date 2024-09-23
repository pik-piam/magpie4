#' @title reportValueTrade
#' @description reports trade value
#' 
#' @importFrom magpiesets reporthelper 
#' @export
#' 
#' @param gdx GDX file
#' @param detail if true, provides estimates for all commodities, otherwhise aggregates some groups
#' @return trade value as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#' 
#'   \dontrun{
#'     x <- reportValueTrade(gdx)
#'   }
#' 

reportValueTrade<-function(gdx,detail=FALSE){
  
  x <- NULL
  
  # net-exports
  out<-tradeValue(gdx, level="regglo", type="net-exports", glo_weight="free_trade")
  out<-reporthelper(x=out,dim = 3.1,level_zero_name = "Trade Value|Net-Exports",detail = FALSE)
  getNames(out) <- paste(getNames(out),"(million US$2017yr)",sep=" ")
  x <- mbind(x,out)

  # gross-exports
  out<-suppressMessages(tradeValue(gdx, level="regglo", type="exports", glo_weight="free_trade"))
  out<-reporthelper(x=out,dim = 3.1,level_zero_name = "Trade Value|Exports",detail = detail)
  getNames(out) <- paste(getNames(out),"(million US$2017/yr)",sep=" ")
  x <- mbind(x,out)
  
  # gross-imports
  out<-suppressMessages(tradeValue(gdx, level="regglo", type="imports", glo_weight="free_trade"))
  out<-reporthelper(x=out,dim = 3.1,level_zero_name = "Trade Value|Imports",detail = detail)
  getNames(out) <- paste(getNames(out),"(million US$2017/yr)",sep=" ")
  x <- mbind(x,out)

  return(x)
}