# reportProduction

reports production

## Usage

``` r
reportProduction(gdx, detail = FALSE, agmip = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=FALSE, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- agmip:

  if agmip = TRUE, additional sector aggregates required for AgMIP are
  reported (e.g. "AGR")

- level:

  aggregation level of returned data ("regglo" by default)

## Value

production as MAgPIE object. Unit: see names

## Production variables

|                                   |          |                                                                                                |
|-----------------------------------|----------|------------------------------------------------------------------------------------------------|
| Name                              | Unit     | Meta                                                                                           |
| Production                        | Mt DM/yr | Total agricultural production                                                                  |
| Production\|+\|Crops              | Mt DM/yr | Production of crops                                                                            |
| Production\|+\|Livestock products | Mt DM/yr | Production of livestock products (excluding fish)                                              |
| Production\|+\|Secondary products | Mt DM/yr | Production of secondary products (processed agricultural goods)                                |
| Production\|+\|Pasture            | Mt DM/yr | Production of pasture biomass                                                                  |
| Production\|+\|Bioenergy crops    | Mt DM/yr | Production of second-generation bioenergy crops (short rotation grasses, short rotation trees) |

## Author

Benjamin Leon Bodirsky, Isabelle Weindl

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportProduction(gdx)
  } # }

```
