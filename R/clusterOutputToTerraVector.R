#' @title Convert cluster output to terra vector
#'
#' @description Enriches land use data on cluster resolution with explicit spatial information by
#' creating a terra polygon for each cluster according to the given clustermap.
#'
#' @param x Landuse data on cluster/cell resolution as a magclass object
#' @param clustermap A dataframe mapping with columns cluster, cell, and country
#' @return A SpatVector with the following columns: c("clusterId", "country", "region", "year", "landtype", "value")
#' @author Pascal Führlich, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' landUse <- magpie4::land("fulldata.gdx", level = "cell")
#' clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
#' clusterPolygons <- magpie4::clusterOutputToTerraVector(landUse, clustermap)
#' terra::writeVector(clusterPolygons, "cluster_resolution.shp")
#' }
#'
#' @export
clusterOutputToTerraVector <- function(x, clustermap) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("terra is required for clusterOutputToTerraVector, please run `install.packages('terra')`")
  }

  stopifnot(identical(names(dimnames(x)), c("j.region", "t", "land")))

  cells <- magclass::getCells(x)
  clusterMagclass <- magclass::new.magpie(cells, names = "clusterId", fill = seq_along(cells))
  clusterMagclass <- madrat::toolAggregate(clusterMagclass, clustermap, from = "cluster", to = "cell")

  clusterPolygons <- terra::as.polygons(magclass::as.SpatRaster(clusterMagclass))
  clusterPolygons <- terra::merge(clusterPolygons, magclass::as.data.frame(x, rev = 3),
                                  by.x = "clusterId", by.y = "region")
  names(clusterPolygons) <- c("clusterId", "region", "year", "landtype", "value")

  clusterIdToCountry <- clustermap$country
  names(clusterIdToCountry) <- as.integer(sub("^[A-Z]{3}\\.", "", clustermap$cluster))
  clusterPolygons$country <- clusterIdToCountry[clusterPolygons$clusterId]
  clusterPolygons <- clusterPolygons[, c("clusterId", "country", "region", "year", "landtype", "value")]

  terra::crs(clusterPolygons) <- "+proj=longlat +datum=WGS84 +no_defs"
  return(clusterPolygons)
}
