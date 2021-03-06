#' @title reportDemandNr
#' @description Similar to reportDemand, but for nitrogen. reports Demand for Food, Feed, Processing, Material, Bioenergy, Seed and Supply Chain Loss
#' 
#' @import magpiesets
#' @export
#' 
#' @param gdx GDX file
#' @param detail if detail=F, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return demand as MAgPIE object (Mt DM)
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportDemand()
#'   }
#' 

reportDemandNr<-function(gdx,detail=FALSE){
  out <- NULL
  x   <-  collapseNames(demand(gdx,level="regglo",attributes="nr"),collapsedim = "attributes")
  getNames(x,dim=1) <- reportingnames(getNames(x,dim=1))
  
  for (type in getNames(x,dim=1)) {
    tmp <- collapseNames(x[,,type],collapsedim = 1)
    # demand.R renamed dim=3.1
    tmp<-reporthelper(x=tmp,level_zero_name = paste0("Demand|",type),detail = detail,dim=3.1)
    getNames(tmp) <- paste(getNames(tmp),"(Mt Nr/yr)",sep=" ")
    out <- mbind(out,tmp)
  }
  
  out <- summationhelper(out)
#  out <- out[,,sort(getNames(out))]
  return(out)
}