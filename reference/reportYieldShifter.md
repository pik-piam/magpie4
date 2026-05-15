# reportYieldShifter

Reports the Crop model input yield shifter

## Usage

``` r
reportYieldShifter(
  gdx,
  file = NULL,
  level = "reg",
  baseyear = "y2000",
  relative = TRUE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- baseyear:

  baseyear for the yield shifter. Also fixes land patterns for
  aggregation to baseyear.

- relative:

  relative or absolute changes to baseyear

## Value

crop yield as MAgPIE object (unit depends on attributes)

## Yield shifter variables

|                                                                |                  |                                                     |
|----------------------------------------------------------------|------------------|-----------------------------------------------------|
| Name                                                           | Unit             | Meta                                                |
| Productivity\|Climate Change Yield Shifter                     | Index baseyear=1 | Climate-induced yield changes relative to base year |
| Productivity\|Climate Change Yield Shifter\|Cereals\|Rainfed   | Index baseyear=1 | Yield shifter for rainfed cereals                   |
| Productivity\|Climate Change Yield Shifter\|Cereals\|Irrigated | Index baseyear=1 | Yield shifter for irrigated cereals                 |

## See also

`reportYieldShifter`

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportYieldShifter(gdx)
  } # }

```
