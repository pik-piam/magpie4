# mappingToLongFormat

Derives a report aggregation mapping (sourceRegion to targetRegion) from
a country-level region mapping. The input must have at least the columns
`countryname`, `country`, and `region`. Any additional columns are
treated as definitions of aggregated macro regions: each unique
non-empty value defines a target region, and all source regions
containing at least one country assigned to that value are mapped to it.

The output mapping has three blocks (each sorted by sourceRegion):

1.  each source region mapped to itself (regional resolution)

2.  each source region mapped to `"GLO"` (global aggregate)

3.  source regions mapped to every additional aggregated region they
    belong to

## Usage

``` r
mappingToLongFormat(mappingOrFileName)
```

## Arguments

- mappingOrFileName:

  Either a data frame containing the country-level region mapping, or a
  character string giving the path to a CSV-file.

## Value

A data frame with columns `sourceRegion` and `targetRegion`.

## Note

This function is a workaround as in magpie4 we can not use the same
mapping formulas that we could use in madrat functions. If the magpie4
aggregation logic is ever rewritten, this function may become obsolete.

## See also

Other Spatial: [`addGeometry()`](addGeometry.md),
[`clusterOutputToTerraVector()`](clusterOutputToTerraVector.md),
[`gdxAggregate()`](gdxAggregate.md),
[`superAggregateX()`](superAggregateX.md)

## Author

Kristine Karstens, Patrick Rein
