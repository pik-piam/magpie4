# reportVegfruitShare

reports the share of livestock products (including fish) in total
calorie food supply

## Usage

``` r
reportVegfruitShare(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

per-capita calories as MAgPIE object (kcal/cap/day)

## Vegfruit share variables

|                                                              |           |                                                               |
|--------------------------------------------------------------|-----------|---------------------------------------------------------------|
| Name                                                         | Unit      | Meta                                                          |
| Nutrition\|Dietary Composition\|Vegetables Fruits Nuts Share | kcal/kcal | Share of vegetables, fruits, and nuts in total calorie supply |

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportLivestockShare(gdx)
  } # }
```
