# reportCostsWithoutIncentives

reports Costs Without Incentives

## Usage

``` r
reportCostsWithoutIncentives(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

magpie object

## Costs without incentives variables

|                                            |                     |                                         |
|--------------------------------------------|---------------------|-----------------------------------------|
| Name                                       | Unit                | Meta                                    |
| Costs Accounting\|Costs without incentives | million US\$2017/yr | Total costs excluding policy incentives |

## Author

David Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportCostsWithoutIncentives(gdx)
} # }

```
