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

yield as MAgPIE object (t DM/ha)

## Details

`Calibrated` is the crop-model input after management calibration but
before technological change; `Calibrated|Including technological change`
additionally applies tau and equals `vm_yld`. Both are weighted with the
1995 cropping pattern held fixed.

## Calibrated input-data yield variables

|                                                                              |         |                                                                         |
|------------------------------------------------------------------------------|---------|-------------------------------------------------------------------------|
| Name                                                                         | Unit    | Meta                                                                    |
| Productivity\|Yields\|Input data\|Calibrated                                 | t DM/ha | Potential crop yields after calibration, before technological change    |
| Productivity\|Yields\|Input data\|Calibrated\|+\|Cereals                     | t DM/ha | Calibrated cereal yields                                                |
| Productivity\|Yields\|Input data\|Calibrated\|Including technological change | t DM/ha | Calibrated potential yields with the tau factor applied (equals vm_yld) |

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportYieldsCropCalib(gdx)
} # }
```
