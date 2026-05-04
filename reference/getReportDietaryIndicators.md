# getReportDietaryIndicators

reports dietary indicators on the country level. These are formatted as
data.frames describing: 1. population, anthropometrics, and intake 2.
caloric intake by food category (without food waste)

## Usage

``` r
getReportDietaryIndicators(gdx, scenario)
```

## Arguments

- gdx:

  filepath of the GDX file

- scenario:

  character string describing the scenario configuration

## Value

list of data.frames for the dietary indicators

## Author

Michael Crawford, Felicitas Beier, Benjamin Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    getReportDietaryIndicators(gdx, scenario)
  } # }
```
