# reportNitrogenBudgetCropland

Reports the Nitrogen Budgets of Croplands for future MAgPIE projections

## Usage

``` r
reportNitrogenBudgetPasture(gdx, include_emissions = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- include_emissions:

  TRUE also divides the N surplus into different emissions

- level:

  aggregation level of returned data ("regglo" by default); use "grid"
  for grid level

## Nitrogen pasture budget variables

|                                                                        |          |                                             |
|------------------------------------------------------------------------|----------|---------------------------------------------|
| Name                                                                   | Unit     | Meta                                        |
| Resources\|Nitrogen\|Pasture Budget\|Inputs                            | Mt Nr/yr | Total nitrogen inputs to pastures           |
| Resources\|Nitrogen\|Pasture Budget\|Inputs\|+\|Manure                 | Mt Nr/yr | Manure nitrogen deposited on pastures       |
| Resources\|Nitrogen\|Pasture Budget\|Inputs\|+\|Atmospheric deposition | Mt Nr/yr | Atmospheric nitrogen deposition on pastures |
| Resources\|Nitrogen\|Pasture Budget\|Withdrawals                       | Mt Nr/yr | Total nitrogen withdrawals from pastures    |
| Resources\|Nitrogen\|Pasture Budget\|Balance                           | Mt Nr/yr | Nitrogen balance on pastures                |
| Resources\|Nitrogen\|Pasture Budget\|Balance\|+\|Surplus               | Mt Nr/yr | Nitrogen surplus on pastures                |

## See also

[`NitrogenBudget`](NitrogenBudget.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportNitrogenBudgetCropland(gdx)
  } # }

```
