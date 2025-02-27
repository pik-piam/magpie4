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
    dem <- mbind(setNames(dimSums(dem,dim = 3),"Timber|Volumetric|Demand|+|Roundwood"),dem)
    getNames(dem) <- paste0(getNames(dem)," (Mm3/yr)")

    prod <- collapseNames(a[,,"Production"])
    getNames(prod) <- paste0("Timber|Volumetric|Production|Roundwood|+|",getNames(prod))
    prod <- mbind(setNames(dimSums(prod,dim = 3),"Timber|Volumetric|Production|+|Roundwood"),prod)
    getNames(prod) <- paste0(getNames(prod)," (Mm3/yr)")

    # heaven <- collapseNames(a[,,"Heaven"])
    # getNames(heaven) <- paste0("Timber|Volumetric|Infeasible|Roundwood|+|",getNames(heaven))
    # heaven <- mbind(setNames(dimSums(heaven,dim = 3),"Timber|Volumetric|Infeasible|+|Roundwood"),heaven)
    # getNames(heaven) <- paste0(getNames(heaven)," (Mm3/yr)")

    netTrade <- collapseNames(a[,,"Net-Trade"])
    getNames(netTrade) <- paste0("Timber|Volumetric|Net-Trade|Roundwood|+|",getNames(netTrade))
    netTrade <- mbind(setNames(dimSums(netTrade,dim = 3),"Timber|Volumetric|Net-Trade|+|Roundwood"),netTrade)
    getNames(netTrade) <- paste0(getNames(netTrade)," (Mm3/yr)")

    exports <- collapseNames(a[,,"Exports"])
    getNames(exports) <- paste0("Timber|Volumetric|Exports|Roundwood|+|",getNames(exports))
    exports <- mbind(setNames(dimSums(exports,dim = 3),"Timber|Volumetric|Exports|+|Roundwood"),exports)
    getNames(exports) <- paste0(getNames(exports)," (Mm3/yr)")

    imports <- collapseNames(a[,,"Imports"])
    getNames(imports) <- paste0("Timber|Volumetric|Imports|Roundwood|+|",getNames(imports))
    imports <- mbind(setNames(dimSums(imports,dim = 3),"Timber|Volumetric|Imports|+|Roundwood"),imports)
    getNames(imports) <- paste0(getNames(imports)," (Mm3/yr)")

    out <- mbind(dem, prod, netTrade, exports, imports)
  } else {message("Disabled (no timber) ", appendLF = FALSE)}

  return(out)
}
