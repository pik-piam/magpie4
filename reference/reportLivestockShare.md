# reportLivestockShare

reports the share of livestock products (including fish) in total
calorie food supply

## Usage

``` r
reportLivestockShare(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

per-capita calories as MAgPIE object (kcal/cap/day)

## Livestock share variables

|                                                 |           |                                                                  |
|-------------------------------------------------|-----------|------------------------------------------------------------------|
| Name                                            | Unit      | Meta                                                             |
| Nutrition\|Dietary Composition\|Livestock Share | kcal/kcal | Share of livestock products (incl. fish) in total calorie supply |

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportLivestockShare(gdx)
  } # }
```
