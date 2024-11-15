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

    dem <- collapseNames(a[,,"Demand"])
    getNames(dem) <- paste0("Timber|Volumetric|Demand|Roundwood|+|",getNames(dem))
    getNames(dem) <- paste0(getNames(dem)," (Mm3/yr)")
    dem <- mbind(setNames(dimSums(dem,dim = 3),"Timber|Volumetric|Demand|+|Roundwood"),dem)

    prod <- collapseNames(a[,,"Production"])
    getNames(prod) <- paste0("Timber|Volumetric|Production|+|Roundwood|",getNames(prod))
    getNames(prod) <- paste0(getNames(prod)," (Mm3/yr)")
    prod <- mbind(setNames(dimSums(prod,dim = 3),"Timber|Volumetric|Production|+|Roundwood"),prod)

    heaven <- collapseNames(a[,,"Heaven"])
    getNames(heaven) <- paste0("Timber|Volumetric|Infeasible|+|Roundwood|",getNames(heaven))
    getNames(heaven) <- paste0(getNames(heaven)," (Mm3/yr)")
    heaven <- mbind(setNames(dimSums(heaven,dim = 3),"Timber|Volumetric|Infeasible|+|Roundwood"),heaven)

    out <- mbind(dem, prod, heaven)
  } else {message("Disabled (no timber) ", appendLF = FALSE)}

  return(out)
}
