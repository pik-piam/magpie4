# reportIntakeDetailed

reports detailed or aggregated per-capita kcal intake including
exogenous scenarios

## Usage

``` r
reportIntakeDetailed(gdx, detail = TRUE, level = "regglo")
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

per-capita calorie intake as MAgPIE object (kcal/cap/day)

## Calorie intake variables

|                                                  |                 |                                                   |
|--------------------------------------------------|-----------------|---------------------------------------------------|
| Name                                             | Unit            | Meta                                              |
| Nutrition\|Calorie Intake                        | kcal/capita/day | Total per-capita calorie intake (excluding waste) |
| Nutrition\|Calorie Intake\|+\|Crops              | kcal/capita/day | Calorie intake from crops                         |
| Nutrition\|Calorie Intake\|+\|Livestock products | kcal/capita/day | Calorie intake from livestock products            |
| Nutrition\|Calorie Intake\|+\|Secondary products | kcal/capita/day | Calorie intake from secondary/processed products  |

## Author

Isabelle Weindl

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportIntakeDetailed(gdx)
  } # }

```
