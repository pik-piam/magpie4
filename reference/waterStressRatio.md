# waterStressRatio

calculates water stress ratio from water availability and water demand
in MAgPIE. Water stress ratio is the ratio of water withdrawals (in the
growing period) to water availability (in the growing period)

## Usage

``` r
waterStressRatio(gdx, file = NULL, level = "cell")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  spatial level of aggregation: "cell" (cellular), "reg" (regional),
  "glo" (global), "regglo" (regional and global) or "grid" (grid cell)

## Value

MAgPIE object

## Author

Felicitas Beier

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- waterStressRatio(gdx)
  } # }
```
