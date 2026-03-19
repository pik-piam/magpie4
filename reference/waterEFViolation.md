# waterEFViolation

calculates environmental flow violation volume from MAgPIE outputs

## Usage

``` r
waterEFViolation(gdx, file = NULL, level = "reg", digits = 4)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  spatial level of aggregation: "cell" (cellular), "reg" (regional),
  "glo" (global), "regglo" (regional and global), or "grid" (for
  disaggregated output using cropland as weight)

- digits:

  integer. For rounding of the return values

## Value

A MAgPIE object containing the volume of environmental flow violations
(km^3)

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- waterEFViolation(gdx)
} # }
```
