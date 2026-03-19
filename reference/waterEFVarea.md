# waterEFVarea

calculates area that falls into cluster experiencing environmental flow
violations from MAgPIE outputs

## Usage

``` r
waterEFVarea(gdx, file = NULL, level = "reg", digits = 4)
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

A MAgPIE object containing the area under environmental flow violations
(Mha)

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- waterEFVarea(gdx)
} # }
```
