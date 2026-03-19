# water_AAI

reads area actually irrigated from a MAgPIE gdx file

## Usage

``` r
water_AAI(gdx, file = NULL, level = "reg")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using
  [`write.magpie`](https://rdrr.io/pkg/magclass/man/write.magpie.html).
  See
  [`write.magpie`](https://rdrr.io/pkg/magclass/man/write.magpie.html)
  for supported file types

- level:

  spatial level of aggregation: "cell" (cellular), "reg" (regional),
  "glo" (global), "regglo" (regional and global) or any other
  aggregation level defined in gdxAggregate

## Value

A MAgPIE object containing the area actually irrigated (Mha)

## Author

Stephen Wirth, Anne Biewald, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- water_AEI(gdx)
} # }
```
