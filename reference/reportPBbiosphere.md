# reportPBbiosphere

reports biosphere planetary boundary: Share of intact land relative to
total land area (unitless) Share of intact land covered by areas within
Global Safety Net (unitless) Share of land area that satisfies landscape
target (unitless)

## Usage

``` r
reportPBbiosphere(gdx, level = "regglo", intactnessRule = "carbon:0.95")
```

## Arguments

- gdx:

  GDX file

- level:

  level of aggregation (regglo: regions and global)

- intactnessRule:

  rule for intact land can be based on percentage of potential carbon
  density reached or on age classes for secondary forests, planted
  forest and other natural land. The argument is split into two
  components: rule: carbon or ageclass threshold: share of carbon
  density reached to be classified as intact or threshold in years can
  be set via this argument Example: "carbon:0.95" or "ageclass:70"

## Value

MAgPIE object

## Biosphere planetary boundary variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Planetary Boundary\|Biosphere\|Share of intact land relative to total land area | unitless | Fraction of intact land (primary forest, mature secondary forest, other natural land) |
| Planetary Boundary\|Biosphere\|Share of intact land covered by areas within Global Safety Net | unitless | Share of intact land in priority conservation areas |
| Planetary Boundary\|Biosphere\|Share of land area that satisfies landscape target | unitless | Share of land where cropland does not exceed 80% of available cropland |

## Author

Patrick von Jeetze, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportPBbiosphere(gdx)
} # }
```
