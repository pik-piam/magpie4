# reportProtein

reports per-capita protein food supply (including household waste)

## Usage

``` r
reportProtein(gdx, detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=F, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

per-capita protein as MAgPIE object (protein/cap/day)

## Protein supply variables

|                                                  |                    |                                                             |
|--------------------------------------------------|--------------------|-------------------------------------------------------------|
| Name                                             | Unit               | Meta                                                        |
| Nutrition\|Protein Supply                        | protein/capita/day | Total per-capita protein supply (including household waste) |
| Nutrition\|Protein Supply\|+\|Crops              | protein/capita/day | Protein supply from crops                                   |
| Nutrition\|Protein Supply\|+\|Livestock products | protein/capita/day | Protein supply from livestock products                      |
| Nutrition\|Protein Supply\|+\|Secondary products | protein/capita/day | Protein supply from secondary/processed products            |

## Author

Benjamin Leon Bodirsky, Kristine Karstens, Abhijeet Mishra, Florian
Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportKcal(gdx)
  } # }

```
