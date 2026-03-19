# wageDevelopment

calculates indicator to describe wage development based on agricultural
wages in MAgPIE (hourly labor costs relative to a base year)

## Usage

``` r
wageDevelopment(gdx, baseYear = 2000, file = NULL, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- baseYear:

  year relative to which the wage development should be calculated

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation ("iso", "reg", "glo", "regglo")

## Value

MAgPIE object containing indicator on wage development

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- wageDevelopment(gdx)
} # }
```
