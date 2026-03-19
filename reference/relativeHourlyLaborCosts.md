# relativeHourlyLaborCosts

calculates labor costs per ag. worker in relation to GDP pc

## Usage

``` r
relativeHourlyLaborCosts(gdx, level = "reg", file = NULL)
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation to report ("iso", "reg", "glo", "regglo", or
  custom region aggregation)

- file:

  a file name the output should be written to using write.magpie

## Value

labor costs per ag. worker in relation to GDP pc

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- relativeHourlyLaborCosts(gdx)
} # }
```
