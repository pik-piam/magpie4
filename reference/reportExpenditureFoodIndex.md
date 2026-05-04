# reportExpenditureFoodIndex

reports food expenditure index and food expenditure index corrected for
emission costs

## Usage

``` r
reportExpenditureFoodIndex(
  gdx,
  baseyear = "y2010",
  basketyear = "y2010",
  level = "regglo"
)
```

## Arguments

- gdx:

  GDX file

- baseyear:

  Baseyear of the price index

- basketyear:

  Year of reference food basket (should be in the past for comparison of
  different runs to have identical and comparable food basket)

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Food expenditure index as MAgPIE object

## Food expenditure index variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Prices\|Food Expenditure Index | Index 2010=100 | Food expenditure index (all food products) |
| Prices\|Food Expenditure Index\|Plant-based food products | Index 2010=100 | Food expenditure index for plant-based products |
| Prices\|Food Expenditure Index\|Livestock food products | Index 2010=100 | Food expenditure index for livestock products |
| Prices\|Food Expenditure Index corrected for ghg costs | Index 2010=100 | Food expenditure index corrected for GHG costs |
| Prices\|Agricultural Primary Products Expenditure Index | Index 2010=100 | Agricultural primary products expenditure index |

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportPriceFoodIndex(gdx)
} # }
```
