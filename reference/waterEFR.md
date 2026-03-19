# waterEFR

reads environmental flow requirements (as they enter MAgPIE) from a
MAgPIE gdx file

## Usage

``` r
waterEFR(gdx, file = NULL, level = "cell", digits = 4)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  spatial level of aggregation: "cell" (cellular), "reg" (regional),
  "glo" (global), "regglo" (regional and global)

- digits:

  integer. For rounding of the return values

## Value

A MAgPIE object containing environmental flow requirements (km^3)

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- waterEFR(gdx)
} # }
```
