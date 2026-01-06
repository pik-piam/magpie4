#' @title reportFeed
#' @description reportes feed demand by animal type
#'
#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return feed demand as MAgPIE object (Mt DM)
#' @author Isabelle Weindl
#' @examples
#'
#'   \dontrun{
#'     x <- reportFeed()
#'   }
#'
#'
#' @section Feed demand variables:
#' Name | Unit | Meta
#' ---|---|---
#' Demand\|Feed\|++\|Feed for Ruminant meat | Mt DM/yr | Feed demand for ruminant meat production
#' Demand\|Feed\|++\|Feed for Dairy | Mt DM/yr | Feed demand for dairy production
#' Demand\|Feed\|++\|Feed for Pig meat | Mt DM/yr | Feed demand for pig meat production
#' Demand\|Feed\|++\|Feed for Poultry meat | Mt DM/yr | Feed demand for poultry meat production
#' Demand\|Feed\|++\|Feed for Eggs | Mt DM/yr | Feed demand for egg production
#' Demand\|Feed\|++\|Feed for Aquaculture | Mt DM/yr | Feed demand for aquaculture
#' @md

#'
reportFeed<-function(gdx,detail=T){
  out <- NULL
  x   <-  feed(gdx,level="regglo", detail=T, nutrient="dm")
  getNames(x,dim=1) <- paste0("feed_",getNames(x,dim=1))
  getNames(x,dim=1) <- reportingnames(getNames(x,dim=1))

  for (type in getNames(x,dim=1)) {
    tmp <- collapseNames(x[,,type],collapsedim = 1)
    tmp<-reporthelper(x=tmp,level_zero_name = paste0("Demand|Feed|",type),detail = detail,dim=3.1)
    getNames(tmp) <- paste(getNames(tmp),"(Mt DM/yr)",sep=" ")
    out <- mbind(out,tmp)
  }

  out <- summationhelper(out)
  #  out <- out[,,sort(getNames(out))]

#include "++"
  lvl1<-c("Demand|Feed|+|Feed for Aquaculture (Mt DM/yr)",
          "Demand|Feed|+|Feed for Poultry meat (Mt DM/yr)",
          "Demand|Feed|+|Feed for Eggs (Mt DM/yr)",
          "Demand|Feed|+|Feed for Dairy (Mt DM/yr)",
          "Demand|Feed|+|Feed for Pig meat (Mt DM/yr)",
          "Demand|Feed|+|Feed for Ruminant meat (Mt DM/yr)" )
  tmp<-out[,,lvl1]
  getNames(tmp)<-sub(dimnames(tmp)[[3]],pattern = "\\+",replacement = "++")
  out<-mbind(tmp,out[,,lvl1,invert=TRUE])

  return(out)
}
