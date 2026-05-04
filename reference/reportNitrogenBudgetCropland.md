# reportNitrogenBudgetCropland

Reports the Nitrogen Budgets of Croplands for future MAgPIE projections

## Usage

``` r
reportNitrogenBudgetCropland(gdx, include_emissions = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- include_emissions:

  TRUE also divides the N surplus into different emissions

- level:

  aggregation level of returned data ("regglo" by default); use "grid"
  for 0.5 degree grid level

## Nitrogen inputs variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Nitrogen\|Cropland Budget\|Inputs | Mt Nr/yr | Total nitrogen inputs to cropland |
| Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|Fertilizer | Mt Nr/yr | Synthetic fertilizer nitrogen applied to cropland |
| Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|Manure | Mt Nr/yr | Manure nitrogen applied to cropland |
| Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|BNF | Mt Nr/yr | Biological nitrogen fixation on cropland |
| Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|Atmospheric deposition | Mt Nr/yr | Atmospheric nitrogen deposition on cropland |
| Resources\|Nitrogen\|Cropland Budget\|Inputs\|+\|Seeds | Mt Nr/yr | Nitrogen from seeds |

## Nitrogen withdrawals variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Nitrogen\|Cropland Budget\|Withdrawals | Mt Nr/yr | Total nitrogen withdrawals from cropland |
| Resources\|Nitrogen\|Cropland Budget\|Withdrawals\|+\|Harvest | Mt Nr/yr | Nitrogen removed in harvested crops |
| Resources\|Nitrogen\|Cropland Budget\|Withdrawals\|+\|Above-ground residues | Mt Nr/yr | Nitrogen in above-ground crop residues |
| Resources\|Nitrogen\|Cropland Budget\|Withdrawals\|+\|Below-ground residues | Mt Nr/yr | Nitrogen in below-ground crop residues |

## Nitrogen balance variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Nitrogen\|Cropland Budget\|Balance | Mt Nr/yr | Total nitrogen balance on cropland |
| Resources\|Nitrogen\|Cropland Budget\|Balance\|+\|Surplus | Mt Nr/yr | Nitrogen surplus on cropland (inputs minus withdrawals) |
| Resources\|Nitrogen\|Cropland Budget\|Balance\|+\|Soil organic matter | Mt Nr/yr | Net nitrogen flow into soil organic matter (negative = release) |
| Resources\|Nitrogen\|Cropland Budget\|Balance\|+\|Balance flow | Mt Nr/yr | Nitrogen balance flow (calibration term) |

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
