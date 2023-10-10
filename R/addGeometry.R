#' @title addGeometry
#'
#' @description Enriches land use data on cluster resolution geometry information as required for conversion
#' by magclass::as.SpatVector
#'
#' @param x Landuse data on cluster/cell resolution as a magclass object
#' @param clustermap A dataframe mapping with columns cluster, cell, and country
#' @return A magclass object enriched with geometry information
#' @author Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' landUse <- magpie4::land("fulldata.gdx", level = "cell")
#' clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
#' landUseEnriched <- magpie4::addGeometry(landUse, clustermap)
#' attr(landUseEnriched, "geometry")
#' attr(landUseEnriched, "crs")
#' }
#'
#' @export

addGeometry <- function(x, clustermap) {
  id <- as.data.frame(magclass::getItems(x, dim = 1, split = TRUE, full = TRUE))
  id[[2]] <- as.integer(id[[2]])
  cells <- magclass::getItems(x, dim = 1)
  clusterMagclass <- magclass::new.magpie(cells, names = "clusterId", fill = id[[2]])
  clusterMagclass <- madrat::toolAggregate(clusterMagclass, clustermap, from = "cluster", to = "cell")
  # 67k clustermap has cells like "-179p75.-16p25.FJI", but after toolAggregate dimnames for dim 1 are
  # "region.region1" despite there being 3 subdimensions & as.SpatRaster expects coordinate subdims to be called x/y
  if (all(grepl("\\..+\\.[A-Z]{3}", clustermap[[1]]))) {
    names(dimnames(clusterMagclass))[1] <- "x.y.country"
  }
  clusterPolygons <- terra::as.polygons(magclass::as.SpatRaster(clusterMagclass))
  terra::crs(clusterPolygons) <- "+proj=longlat +datum=WGS84 +no_defs"
  m <- magclass::as.magpie(clusterPolygons)
  attr(x, "geometry") <- attr(m, "geometry")
  attr(x, "crs") <- attr(m, "crs")
  return(x)
}
