#' @title reportProcessing
#' @description reportes processing input and output quantities primary-to-process or primary-to-secondary
#' 
#' @import magpiesets
#' @export
#' 
#' @param gdx GDX file
#' @param indicator primary_to_process or secondary_from_primary
#' @param detail if detail=FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @indicator "primary_to_process" for process or "secondary_from_primary" for secondary product output
#' @return processing demand as MAgPIE object (Mt DM)
#' @author David Chen, Benjamin Leon Bodirsky
#' @importFrom magclass dimOrder
#' @importFrom magpiesets findset
#' @examples
#' 
#'   \dontrun{
#'     x <- reportProcessing(gdx=gdx, detail=T, indicator = "primary_to_process")
#'   }
#' 
#' 
reportProcessing<-function(gdx, detail=T, indicator = "primary_to_process"){
  out <- NULL
  
  if (indicator == "primary_to_process"){
    
    x   <-  processing(gdx,level="regglo", indicator = "primary_to_process")
    
    
    getNames(x,dim=1)<-reportingnames(getNames(x,dim=1))
    a <- dimSums(x, dim=3.2)
    x<-reporthelper(x=x,dim = 3.2,level_zero_name = "",detail = detail)
    getNames(x) <- sub(getNames(x),pattern = "\\.",replacement = "|")
    getSets= sub(getSets(x),pattern = "\\.",replacement = "|")
    getNames(x) <- paste0("Demand|Processing|",getNames(x))
    getNames(a) <- paste0("Demand|Processing|",getNames(a))
    out <- mbind(a, x)
    
    out=out[,,which(dimSums(out,dim=c(1,2))>0)]
    
    out <- summationhelper(out, sep="+")
    
    getNames(out) <- sub(getNames(out),pattern = "\\.",replacement = "|")
    getNames(out) <- paste(getNames(out),"(Mt DM/yr)",sep=" ")
    getNames(out) <- sub(getNames(out), pattern = "Processing|+", replacement = "Processing|++", fixed=TRUE)
    
    return(out)
  }
  
  else if (indicator == "secondary_from_primary"){
    
    x   <-  processing(gdx,level="regglo", indicator = "secondary_from_primary")
    x<-dimOrder(x,c(2,1))
    getNames(x,dim=2) <- reportingnames(getNames(x,dim=2))
    
    
    x<-reporthelper(x=x,level_zero_name = paste0("Production"),detail = detail,dim=3.1)
    getNames(x) <- sub(getNames(x),pattern = "\\.",replacement = "|")
    getNames(x) <- paste(getNames(x),"(Mt DM/yr)",sep=" ")
    out <- summationhelper(x)
    
    
    return(out)
  }
}

