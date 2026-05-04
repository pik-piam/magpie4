# reportYieldsCropCalib

reports potential yields after calibration

## Usage

``` r
reportYieldsCropCalib(gdx, detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=FALSE, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- level:

  aggregation level of returned data ("regglo" by default)

## Value

yield as MAgPIE object (Mt DM/ha)

## Calibrated yield variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Productivity\|Yield (after calibration) | t DM/ha | Potential crop yields after calibration |
| Productivity\|Yield (after calibration)\|+\|Cereals | t DM/ha | Calibrated cereal yields |
| Productivity\|Yield (including tau) | t DM/ha | Potential yields including tau factor |

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportYieldsCropCalib(gdx)
} # }
```
