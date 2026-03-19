# water_avail

reads available water from a MAgPIE gdx file

## Usage

``` r
water_avail(
  gdx,
  file = NULL,
  level = "reg",
  sources = NULL,
  sum = TRUE,
  digits = 4
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  spatial level of aggregation: "cell" (cellular), "reg" (regional),
  "glo" (global), "regglo" (regional and global) or any other
  aggregation level defined in superAggregate

- sources:

  Vector of water sources that shall be obtained. NULL for all sources

- sum:

  Sum the contribution of different sources (TRUE) or display them
  individually (FALSE)

- digits:

  integer. For rounding of the return values

## Value

A MAgPIE object containing the available water (km^3)

## Author

Markus Bonsch, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- water_avail(gdx)
} # }
```
