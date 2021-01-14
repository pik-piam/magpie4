#' @title reportTimber
#' @description reports MAgPIE demand for timber.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return Timber demand
#' @author Abhijeet Mishra
#' @examples
#' 
#'   \dontrun{
#'     x <- reportTimber(gdx)
#'   }
#' 

reportTimber<-function(gdx){
  a <- NULL
  
  if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
    a <- Timber(gdx,level = "regglo")
    getNames(a,dim=2) <- reportingnames(getNames(a,dim=2))
    a <- add_columns(x = a,dim = 3.2,addnm = "Roundwood")
    a[,,"Roundwood"] <- a[,,"Industrial roundwood"] + a[,,"Wood fuel"]
    
    dem <- collapseNames(a[,,"Demand"])
    getNames(dem) <- paste0("Timber|Volumetric|Demand|",getNames(dem))
    getNames(dem) <- paste0(getNames(dem)," (Mm3/yr)")
    
    prod <- collapseNames(a[,,"Production"])
    getNames(prod) <- paste0("Timber|Volumetric|Production|",getNames(prod))
    getNames(prod) <- paste0(getNames(prod)," (Mm3/yr)")
    
    out <- mbind(dem,prod)
  } else {cat("Disabled for magpie run without dynamic forestry. ")}
  
  return(out)
}