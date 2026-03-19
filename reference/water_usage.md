# water_usage

reads area usage from a MAgPIE gdx file

## Usage

``` r
water_usage(
  gdx,
  file = NULL,
  level = "reg",
  users = NULL,
  sum = FALSE,
  seasonality = "total",
  abstractiontype = "withdrawal",
  digits = 4
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  spatial level of aggregation: "grid" (grid-cell) "cell" (cellular),
  "reg" (regional), "glo" (global), "regglo" (regional and global) or
  any other aggregation level defined in gdxAggregate

- users:

  NULL or "sectors" or "kcr" or "kli". If NULL, all sectors including
  crop-wise water use and livestock will be obtained. If sectors, will
  only report for high-level sectors - agriculture, industry,
  electricity, domestic, ecosystem. Sum is applicable only in the case
  of sectors

- sum:

  determines whether output should be sector specific (FALSE) or
  aggregated over all sectors (TRUE)

- seasonality:

  water usage time of the year. options: "grper" (growing period) or
  "total" (entire year). Note: currently only implemented for
  non-agricultural water usage.

- abstractiontype:

  water usage abstraction type: "withdrawal" or "consumption"

- digits:

  integer. For rounding of the return values

## Value

A MAgPIE object containing the water usage (km^3/yr)

## Author

Markus Bonsch, Vartika Singh, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- water_usage(gdx)
} # }
```
