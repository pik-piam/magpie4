#' @title reportWaterUsage
#' @description reports water usage for agricultural sector, crops and livestock
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param detail logical. Setting to FALSE reports for agricultural sector, TRUE reports for combined, crops and livestock separately
#' @return water usage as MAgPIE object Unit: see names
#' @author Florian Humpenoeder, Vartika Singh, Miodrag Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- reportWaterUsage(gdx)
#'   }
#' 


reportWaterUsage<-function(gdx, detail=TRUE) {
  
  x <- water_usage(gdx, level="regglo",users="sectors", sum=FALSE,digits=3)[,,"agriculture"]
  getNames(x) <- "Resources|Water|Withdrawal|Agriculture (km3/yr)"
  
  if (detail==TRUE) {
   
      y <- water_usage(gdx,level="regglo",users="kcr",sum=FALSE,digits=3)
      out<-reporthelper(x=y,dim=3.1,level_zero_name = "Resources|Water|Withdrawal|Agriculture|Crops",detail = detail)
      getNames(out) <- paste(gsub("\\.","|",getNames(out)),"(km3/yr)",sep=" ")
      out <- summationhelper(out,sep = "+")
    
    x <- mbind(x,out)
    
    z <- water_usage(gdx,level="regglo",users="kli",sum=FALSE,digits=3)
    out<-reporthelper(x=z,dim=3.1,level_zero_name = "Resources|Water|Withdrawal|Agriculture|Livestock",detail = detail)
    getNames(out) <- paste(gsub("\\.","|",getNames(out)),"(km3/yr)",sep=" ")
    out <- summationhelper(out,sep = "+")
    
    x <- mbind(x,out)
     }
  return(x)
} 
  
  
 
  
 
