# reportDemand

reports Demand for Food, Feed, Processing, Material, Bioenergy, Seed and
Supply Chain Loss

## Usage

``` r
reportDemand(gdx, detail = FALSE, agmip = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=F, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- agmip:

  if agmip=T, additional sector aggregates required for agmip are
  reported (e.g. "AGR")

- level:

  The level at which the report data should be aggregated.

## Value

demand as MAgPIE object (Mt DM)

## Demand variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Demand | Mt DM/yr | Total demand for agricultural products including food, feed, processing, material, bioenergy, seed and supply chain loss |
| Demand\|++\|Crops | Mt DM/yr | Demand for crops including food, feed products and bioenergy (1st and 2nd generation crops) |
| Demand\|++\|Livestock products | Mt DM/yr | Demand for livestock products (excluding fish) |
| Demand\|++\|Secondary products | Mt DM/yr | Demand for secondary products (processed agricultural goods) |
| Demand\|++\|Pasture | Mt DM/yr | Demand for pasture biomass |
| Demand\|++\|Bioenergy crops | Mt DM/yr | Demand for second-generation bioenergy crops |

## Demand by use variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Demand\|Food | Mt DM/yr | Demand for food consumption |
| Demand\|Feed | Mt DM/yr | Demand for animal feed |
| Demand\|Processing | Mt DM/yr | Demand for food processing |
| Demand\|Material | Mt DM/yr | Demand for material use (non-food, non-feed) |
| Demand\|Bioenergy | Mt DM/yr | Demand for bioenergy production |
| Demand\|Seed | Mt DM/yr | Demand for seeds |
| Demand\|Supply Chain Loss | Mt DM/yr | Losses in the supply chain |

## Author

Benjamin Leon Bodirsky, Isabelle Weindl

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportDemand()
  } # }

```
