# water_AEI

reads area equipped for irrigation from a MAgPIE gdx file

## Usage

``` r
water_AEI(gdx, file = NULL, level = "reg")
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

## Value

A MAgPIE object containing the area equipped for irrigation (Mha)

## Author

Markus Bonsch

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- water_AEI(gdx)
  } # }
```
