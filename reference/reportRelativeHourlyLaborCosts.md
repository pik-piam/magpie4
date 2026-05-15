# reportRelativeHourlyLaborCosts

reports labor costs per ag. worker in relation to GDP pc from MAgPIE
results

## Usage

``` r
reportRelativeHourlyLaborCosts(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

labor costs per ag. worker in relation to GDP pc as MAgPIE object

## Relative hourly labor cost variables

|                                                         |      |                                                     |
|---------------------------------------------------------|------|-----------------------------------------------------|
| Name                                                    | Unit | Meta                                                |
| Labor\|Wages\|Labor costs per worker relative to GDP pc | %    | Agricultural labor costs relative to GDP per capita |

## Author

Debbora Leip

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportRelativeHourlyLaborCosts(gdx)
  } # }

```
