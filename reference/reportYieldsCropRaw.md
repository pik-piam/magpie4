# reportYieldsCropRaw

reports potential yields before calibration

## Usage

``` r
reportYieldsCropRaw(gdx, detail = FALSE, level = "regglo")
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

Uncalibrated potential yields as they come from the crop model, before
management calibration and technological change. Weighted with the 1995
cropping pattern held fixed.

## Uncalibrated input-data yield variables

|                                                            |         |                                          |
|------------------------------------------------------------|---------|------------------------------------------|
| Name                                                       | Unit    | Meta                                     |
| Productivity\|Yields\|Input data\|Uncalibrated             | t DM/ha | Potential crop yields before calibration |
| Productivity\|Yields\|Input data\|Uncalibrated\|+\|Cereals | t DM/ha | Uncalibrated cereal yields               |

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportYieldsCropRaw(gdx)
} # }
```
