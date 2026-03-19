# hourlyLaborCosts

returns hourly labor costs in agriculture from MAgPIE results

## Usage

``` r
hourlyLaborCosts(gdx, level = "reg", file = NULL)
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation to report employment ("iso", "reg", "glo", or
  "regglo")

- file:

  a file name the output should be written to using write.magpie

## Value

hourly labor costs in agriculture

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- hourlyLaborCosts(gdx)
} # }
```
