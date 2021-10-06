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
  
  x <- water_usage(gdx,level="regglo",users=NULL,sum=FALSE,digits=3)[,,"agriculture"]
  getNames(x) <- "Resources|Water|Withdrawal|Agriculture (km3/yr)"
  return(x)
  
  if (detail==TRUE) {
    out <-x 
    
    y <- water_usage(gdx,level="regglo",users="kcr",sum=FALSE,digits=3)[,,"agriculture"]
    getNames(y) <- "Resources|Water|Withdrawal|Agriculture|Crops (km3/yr)"
    out <- mbind(out,y)
    
    z <- water_usage(gdx,level="regglo",users="kli",sum=FALSE,digits=3)[,,"agriculture"]
    getNames(z) <- "Resources|Water|Withdrawal|Agriculture|Livestock (km3/yr)"
    out <- mbind(out,z)
    
    return(out)
    
  } 
} 
  
  
 
  
 
