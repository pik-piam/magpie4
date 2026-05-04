# reportProductionBioenergy

reports 2nd gen bioenergy production

## Usage

``` r
reportProductionBioenergy(gdx, detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=FALSE, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

production as MAgPIE object. Unit: see names

## Bioenergy production variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Production\|Bioenergy\|2nd generation | EJ/yr | Second generation bioenergy production (grassy and woody crops) |
| Production\|Bioenergy\|2nd generation\|++\|Grassy bioenergy crops | EJ/yr | Production from short rotation grasses |
| Production\|Bioenergy\|2nd generation\|++\|Woody bioenergy crops | EJ/yr | Production from short rotation trees |
| Production\|Bioenergy\|2nd generation\|Cumulative | EJ | Cumulative second generation bioenergy production |

## Author

Florian Humpenoeder

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportProductionBioenergy(gdx)
  } # }
```
