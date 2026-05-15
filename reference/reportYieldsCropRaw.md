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

yield as MAgPIE object (Mt DM/ha)

## Raw yield variables

|                                                      |         |                                          |
|------------------------------------------------------|---------|------------------------------------------|
| Name                                                 | Unit    | Meta                                     |
| Productivity\|Yield (before calibration)             | t DM/ha | Potential crop yields before calibration |
| Productivity\|Yield (before calibration)\|+\|Cereals | t DM/ha | Uncalibrated cereal yields               |

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportYieldsCropRaw(gdx)
} # }
```
