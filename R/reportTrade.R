#' @title reportTrade
#' @description reports trade
#' 
#' @import magpiesets
#' @export
#' 
#' @param gdx GDX file
#' @param detail if true, provides estimates for all commodities, otherwhise aggregates some groups
#' @return Net-Exports and self sufficiency (exports/domestic supply) as MAgPIE object. Unit: see names
#' @author Benjamin Leon Bodirsky, Mishko Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- reportTrade(gdx="fulldata.gdx",detail=TRUE)
#'   }
#' 

reportTrade<-function(gdx,detail=FALSE){
  x <- NULL
  # net-exports
  out<-trade(gdx,level = "regglo",type = "net-exports")
  out<-reporthelper(x=out,dim = 3.1,level_zero_name = "Trade|Net-Trade", detail = detail)
  out <- add_columns(out,addnm = "Trade|Net-Trade|Crops",dim = 3.1)
  out[,,"Trade|Net-Trade|Crops"] <- dimSums(out[,,grep(pattern = "Crops\\|",x = getNames(out))],dim = 3)
  getNames(out) <- paste(getNames(out),"(Mt DM/yr)",sep=" ")
  x <- mbind(x,out)
  x <- summationhelper(x)
  # # gross exports
  # out<-trade(gdx,level = "regglo",type = "exports")
  # out<-reporthelper(x=out,dim = 3.1,level_zero_name = "Agriculture|Trade|Exports",detail = detail)
  # getNames(out) <- paste(getNames(out),"(Mt DM/yr)",sep=" ")
  # x <- mbind(x,out)
  # 
  # # gross imports
  # out<-trade(gdx,level = "regglo",type = "imports")
  # out<-reporthelper(x=out,dim = 3.1,level_zero_name = "Agriculture|Trade|Imports",detail = detail)
  # getNames(out) <- paste(getNames(out),"(Mt DM/yr)",sep=" ")
  # x <- mbind(x,out)
  
  # self_sufficiency
  self_suff<-suppressMessages(trade(gdx,level = "regglo",relative=T,weight=T))
  weight<-self_suff$weight
  self_suff<-self_suff$x
  out<-(
    reporthelper(x=self_suff*weight,dim = 3.1,level_zero_name = "Trade|Self-sufficiency",detail = detail)
    / reporthelper(x=weight,dim = 3.1,level_zero_name = "Trade|Self-sufficiency",detail = detail)
  )
  getNames(out) <- paste(getNames(out),"(1)",sep=" ")
  x <- mbind(x,out)
  
  return(x)
}