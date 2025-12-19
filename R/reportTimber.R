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
#'
#' @section Timber demand variables:
#' Name | Unit | Meta
#' ---|---|---
#' Timber\\|Volumetric\\|Demand\\|+\\|Roundwood | Mm3/yr | Total roundwood demand
#' Timber\\|Volumetric\\|Demand\\|Roundwood\\|+\\|Industrial roundwood | Mm3/yr | Demand for industrial roundwood
#' Timber\\|Volumetric\\|Demand\\|Roundwood\\|+\\|Wood fuel | Mm3/yr | Demand for wood fuel
#' Timber\\|Volumetric\\|Demand\\|Roundwood\\|+\\|Timber for construction | Mm3/yr | Demand for timber used in construction
#'
#' @section Timber production variables:
#' Name | Unit | Meta
#' ---|---|---
#' Timber\\|Volumetric\\|Production\\|+\\|Roundwood | Mm3/yr | Total roundwood production
#' Timber\\|Volumetric\\|Production\\|Roundwood\\|+\\|Industrial roundwood | Mm3/yr | Production of industrial roundwood
#' Timber\\|Volumetric\\|Production\\|Roundwood\\|+\\|Wood fuel | Mm3/yr | Production of wood fuel
#' Timber\\|Volumetric\\|Production\\|Roundwood\\|+\\|Timber for construction | Mm3/yr | Production of timber for construction
#'
#' @section Timber trade variables:
#' Name | Unit | Meta
#' ---|---|---
#' Timber\\|Volumetric\\|Net-Trade\\|+\\|Roundwood | Mm3/yr | Net export of roundwood
#' Timber\\|Volumetric\\|Net-Trade\\|Roundwood\\|+\\|Industrial roundwood | Mm3/yr | Net export of industrial roundwood
#' Timber\\|Volumetric\\|Net-Trade\\|Roundwood\\|+\\|Wood fuel | Mm3/yr | Net export of wood fuel
#' Timber\\|Volumetric\\|Net-Trade\\|Roundwood\\|+\\|Timber for construction | Mm3/yr | Net export of timber for construction
#' Timber\\|Volumetric\\|Exports\\|+\\|Roundwood | Mm3/yr | Gross exports of roundwood
#' Timber\\|Volumetric\\|Exports\\|Roundwood\\|+\\|Industrial roundwood | Mm3/yr | Gross exports of industrial roundwood
#' Timber\\|Volumetric\\|Exports\\|Roundwood\\|+\\|Wood fuel | Mm3/yr | Gross exports of wood fuel
#' Timber\\|Volumetric\\|Exports\\|Roundwood\\|+\\|Timber for construction | Mm3/yr | Gross exports of timber for construction
#' Timber\\|Volumetric\\|Imports\\|+\\|Roundwood | Mm3/yr | Gross imports of roundwood
#' Timber\\|Volumetric\\|Imports\\|Roundwood\\|+\\|Industrial roundwood | Mm3/yr | Gross imports of industrial roundwood
#' Timber\\|Volumetric\\|Imports\\|Roundwood\\|+\\|Wood fuel | Mm3/yr | Gross imports of wood fuel
#' Timber\\|Volumetric\\|Imports\\|Roundwood\\|+\\|Timber for construction | Mm3/yr | Gross imports of timber for construction
#' @md


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
