# waterEFVratio

calculates ratio of environmental flow violation volume (EFV) to water
environmental flow requirements (EFR) in MAgPIE.

## Usage

``` r
waterEFVratio(gdx, file = NULL, level = "cell")
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
    x <- waterEFVratio(gdx)
  } # }
```
