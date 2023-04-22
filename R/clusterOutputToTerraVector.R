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

  # extract spatial IDs
  id <- as.data.frame(magclass::getItems(x, dim = 1, split = TRUE, full = TRUE))
  id[[2]] <- as.integer(id[[2]])

  # generate polygons
  cells <- magclass::getItems(x, dim = 1)
  clusterMagclass <- magclass::new.magpie(cells, names = "clusterId", fill = id[[2]])
  clusterMagclass <- madrat::toolAggregate(clusterMagclass, clustermap, from = "cluster", to = "cell")
  clusterPolygons <- terra::as.polygons(magclass::as.SpatRaster(clusterMagclass))
  terra::crs(clusterPolygons) <- "+proj=longlat +datum=WGS84 +no_defs"

  # fill in data
  data  <- cbind(id, as.data.frame(magclass::wrap(as.array(x), list(1, 2:3), sep = "..")))
  clusterPolygons <- terra::merge(clusterPolygons, data, by.x = "clusterId", by.y = "region")

  return(clusterPolygons)
}
