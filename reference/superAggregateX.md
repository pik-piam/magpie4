# superAggregateX

drop-in replacement for superAggregate based on toolAggregate

## Usage

``` r
superAggregateX(
  data,
  aggr_type,
  level = "reg",
  weight = NULL,
  crop_aggr = FALSE
)
```

## Arguments

- data:

  A MAgPIE object

- aggr_type:

  Aggregation Type. Can be any function for one or two objects (data and
  weight) of the same size. Currently pre-supported functions:
  "sum","mean","weighted_mean".

- level:

  Either a level or the name of a mapping file. Allowed level types are
  global "glo", regional "reg" and "regglo". The mapping file can only
  map from regions to other regions.

- weight:

  Currently only used for weighted_mean

- crop_aggr:

  determines whether output should be crop-specific (FALSE) or
  aggregated over all crops (TRUE). The method used for aggregation is
  set by aggr_type

## Value

returns a MAgPIE object.

## See also

Other Spatial: [`addGeometry()`](addGeometry.md),
[`clusterOutputToTerraVector()`](clusterOutputToTerraVector.md),
[`gdxAggregate()`](gdxAggregate.md),
[`mappingToLongFormat()`](mappingToLongFormat.md)

## Author

Jan Philipp Dietrich
