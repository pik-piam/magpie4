# addGeometry

Enriches land use data on cluster resolution geometry information as
required for conversion by magclass::as.SpatVector

## Usage

``` r
addGeometry(x, clustermap)
```

## Arguments

- x:

  Landuse data on cluster/cell resolution as a magclass object

- clustermap:

  A dataframe mapping with columns cluster, cell, and optionally country

## Value

A magclass object enriched with geometry information

## See also

Other Spatial:
[`clusterOutputToTerraVector()`](clusterOutputToTerraVector.md),
[`gdxAggregate()`](gdxAggregate.md),
[`mappingToLongFormat()`](mappingToLongFormat.md),
[`superAggregateX()`](superAggregateX.md)

## Author

Jan Philipp Dietrich, Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
landUse <- magpie4::land("fulldata.gdx", level = "cell")
clustermap <- readRDS(Sys.glob("clustermap_*.rds"))
landUseEnriched <- magpie4::addGeometry(landUse, clustermap)
attr(landUseEnriched, "geometry")
attr(landUseEnriched, "crs")
} # }
```
