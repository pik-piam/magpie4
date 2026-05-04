# reportTrade

reports trade

## Usage

``` r
reportTrade(gdx, detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if true, provides estimates for all commodities, otherwise aggregates
  some groups

- level:

  The aggregation level of the trade reporting

## Value

Net-Exports and self sufficiency (exports/domestic supply) as MAgPIE
object. Unit: see names

## Net trade variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Trade\|Net-Trade\|+\|Crops | Mt DM/yr | Net export of crops (positive = net exporter) |
| Trade\|Net-Trade\|+\|Livestock products | Mt DM/yr | Net export of livestock products (excluding fish) |
| Trade\|Net-Trade\|+\|Secondary products | Mt DM/yr | Net export of secondary products |
| Trade\|Net-Trade\|+\|Bioenergy crops | Mt DM/yr | Net export of bioenergy crops |

## Gross trade variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Trade\|Exports\|+\|Crops | Mt DM/yr | Gross exports of crops |
| Trade\|Exports\|+\|Livestock products | Mt DM/yr | Gross exports of livestock products |
| Trade\|Imports\|+\|Crops | Mt DM/yr | Gross imports of crops |
| Trade\|Imports\|+\|Livestock products | Mt DM/yr | Gross imports of livestock products |

## Self-sufficiency variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Trade\|Self-sufficiency\|+\|Crops | 1 | Self-sufficiency ratio for crops (production/domestic supply) |
| Trade\|Self-sufficiency\|+\|Livestock products | 1 | Self-sufficiency ratio for livestock products |

## Author

Benjamin Leon Bodirsky, Mishko Stevanovic

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportTrade(gdx="fulldata.gdx",detail=TRUE)
  } # }
```
