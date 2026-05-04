# reportKcal

reports per-capita calories food supply (including household waste)

## Usage

``` r
reportKcal(gdx, detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=F, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- level:

  spatial aggregation: "reg", "glo", "regglo", "iso"

## Value

per-capita calories as MAgPIE object (kcal/cap/day)

## Calorie supply variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Nutrition\|Calorie Supply | kcal/capita/day | Total per-capita calorie supply (including household waste) |
| Nutrition\|Calorie Supply\|+\|Crops | kcal/capita/day | Calorie supply from crops |
| Nutrition\|Calorie Supply\|+\|Livestock products | kcal/capita/day | Calorie supply from livestock products |
| Nutrition\|Calorie Supply\|+\|Secondary products | kcal/capita/day | Calorie supply from secondary/processed products |

## Author

Benjamin Leon Bodirsky, Kristine karstens, Abhijeet Mishra

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportKcal(gdx)
  } # }

```
