# reportWageDevelopment

reports indicator on wage development: hourly labor costs in each time
step relative to hourly labor costs in 2000

## Usage

``` r
reportWageDevelopment(gdx, baseYear = 2000, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- baseYear:

  year relative to which the wage development should be calculated

- level:

  spatial aggregation: "reg", "glo", "regglo"

## Value

indicator on wage development as MAgPIE object

## Wage development variables

|                                                   |       |                                                     |
|---------------------------------------------------|-------|-----------------------------------------------------|
| Name                                              | Unit  | Meta                                                |
| Labor\|Wages\|Hourly labor costs relative to 2000 | index | Hourly labor cost development relative to base year |

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportWageDevelopment(gdx)
} # }
```
