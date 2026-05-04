# reportHourlyLaborCosts

reports hourly labor costs in agriculture from MAgPIE results

## Usage

``` r
reportHourlyLaborCosts(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation: "reg", "glo", "regglo"

## Value

hourly labor costs as MAgPIE object

## Hourly labor cost variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Labor\|Wages\|Hourly labor costs | US\$2017/h | Hourly labor costs in agriculture |

## Author

Debbora Leip

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportHourlyLaborCosts(gdx)
  } # }

```
