# reportFoodExpenditure

reports per-capita calories food supply (including household waste)

## Usage

``` r
reportFoodExpenditure(gdx, detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=F, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- level:

  spatial aggregation: "reg", "glo", "regglo", "iso"

## Value

per-capita calories as MAgPIE object (kcal/cap/day)

## Food expenditure variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Household Expenditure\|Agricultural Primary Products\|Expenditure | US\$2017/capita | Per-capita expenditure on agricultural primary products |
| Household Expenditure\|Agricultural Primary Products\|Expenditure Share | US\$2017/US\$2017 | Share of income spent on agricultural primary products |
| Household Expenditure\|Food\|Expenditure | US\$2017/capita | Per-capita food expenditure (value added) |
| Household Expenditure\|Food\|Expenditure Share | US\$2017/capita | Share of income spent on food (value added) |

## Author

Benjamin Leon Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportFoodExpenditure(gdx)
  } # }

```
