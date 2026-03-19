# reportPriceShock

Reports the change in consumption and expenditure due to higher or lower
food prices

## Usage

``` r
reportPriceShock(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

magpie object

## Price shock variables

|                                                                                 |              |                  |
|---------------------------------------------------------------------------------|--------------|------------------|
| Name                                                                            | Unit         | Meta             |
| Food Supply\|Calorie Supply\|Price Induced Change\|Absolute\|Total Calories     | kcal/cap/day | Absolute total   |
| Food Supply\|Calorie Supply\|Price Induced Change\|Absolute\|Livestock Calories | kcal/cap/day | Livestock        |
| Food Supply\|Calorie Supply\|Price Induced Change\|Relative\|Total Calories     | kcal/kcal    | Relative total   |
| Household Expenditure\|Food\|Price Induced Change\|Absolute\|Food Expenditure   | USD/cap      | Absolute expense |
| Household Expenditure\|Food\|Price Induced Change\|Relative\|Food Expenditure   | USD/USD      | Relative expense |

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportPriceShock(gdx)
  } # }
```
