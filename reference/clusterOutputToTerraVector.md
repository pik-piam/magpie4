# Convert cluster output to terra vector

Enriches land use data on cluster resolution with explicit spatial
information by creating a terra polygon for each cluster according to
the given clustermap.

## Usage

``` r
clusterOutputToTerraVector(x, clustermap)
```

## Arguments

- x:

  Landuse data on cluster/cell resolution as a magclass object

- clustermap:

  A dataframe mapping with columns cluster, cell, and country

## Value

A SpatVector with the following columns: c("clusterId", "country",
"region", "year", "landtype", "value")

## See also

Other Spatial: [`addGeometry()`](addGeometry.md),
[`gdxAggregate()`](gdxAggregate.md),
[`mappingToLongFormat()`](mappingToLongFormat.md),
[`superAggregateX()`](superAggregateX.md)

## Author

Pascal Führlich, Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
landUse <- magpie4::land("fulldata.gdx", level = "cell")
clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
clusterPolygons <- magpie4::clusterOutputToTerraVector(landUse, clustermap)
terra::writeVector(clusterPolygons, "cluster_resolution.shp")
} # }
```
