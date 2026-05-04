# reportNitrogenBudgetNonagland

Reports the Nitrogen Budgets of non-agricultural lands for future MAgPIE
projections

## Usage

``` r
reportNitrogenBudgetNonagland(gdx, level = "reg")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("reg" by default); use "grid" for
  grid level

## Non-agricultural nitrogen budget variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Nitrogen\|Non-Agricultural Land Budget\|Balance | Mt Nr/yr | Nitrogen surplus on non-agricultural land |
| Resources\|Nitrogen\|Non-Agricultural Land Budget\|Inputs | Mt Nr/yr | Total nitrogen inputs to non-agricultural land |
| Resources\|Nitrogen\|Non-Agricultural Land Budget\|Inputs\|+\|Fixation | Mt Nr/yr | Biological N fixation on non-ag land |
| Resources\|Nitrogen\|Non-Agricultural Land Budget\|Inputs\|+\|Deposition | Mt Nr/yr | Atmospheric N deposition on non-ag land |

## See also

[`NitrogenBudget`](NitrogenBudget.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportNitrogenBudgetNonagland(gdx)
  } # }

```
