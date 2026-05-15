# reportYields

reports yields

## Usage

``` r
reportYields(gdx, detail = FALSE, physical = TRUE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=FALSE, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- physical:

  if true (default) physical area (croparea) used for yield calculation;
  if false harvested area used for yield calculation

- level:

  aggregation level of returned data ("regglo" by default)

## Value

yield as MAgPIE object (Mt DM/ha)

## Yield variables

|                                            |         |                                                                                             |
|--------------------------------------------|---------|---------------------------------------------------------------------------------------------|
| Name                                       | Unit    | Meta                                                                                        |
| Productivity\|Yield                        | t DM/ha | Crop yields calculated as production divided by physical cropland area                      |
| Productivity\|Yield\|+\|Crops              | t DM/ha | Yield of all crops                                                                          |
| Productivity\|Yield\|Crops\|+\|Cereals     | t DM/ha | Yield of cereals (maize, rice, temperate cereals and tropical cereals)                      |
| Productivity\|Yield\|Crops\|+\|Oil crops   | t DM/ha | Yield of oil crops (cotton seed, groundnuts, oilpalms, other oil crops, soybean, sunflower) |
| Productivity\|Yield\|Crops\|+\|Sugar crops | t DM/ha | Yield of sugar crops (sugar beet, sugar cane)                                               |
| Productivity\|Yield\|Crops\|+\|Other crops | t DM/ha | Yield of other crops (fruits, vegetables, nuts, potatoes, pulses, tropical roots)           |
| Productivity\|Yield\|+\|Pasture            | t DM/ha | Yield of pasture biomass                                                                    |
| Productivity\|Yield\|++\|Irrigated         | t DM/ha | Yield on irrigated cropland                                                                 |
| Productivity\|Yield\|++\|Rainfed           | t DM/ha | Yield on rainfed cropland                                                                   |

## Yield by harvested area variables

|                                                          |         |                                                                         |
|----------------------------------------------------------|---------|-------------------------------------------------------------------------|
| Name                                                     | Unit    | Meta                                                                    |
| Productivity\|Yield by harvested area                    | t DM/ha | Crop yields calculated as production divided by harvested cropland area |
| Productivity\|Yield by harvested area\|+\|Crops          | t DM/ha | Yield by harvested area of all crops                                    |
| Productivity\|Yield by harvested area\|Crops\|+\|Cereals | t DM/ha | Yield by harvested area of cereals                                      |

## Author

Florian Humpenoeder, Xiaoxi Wang, Kristine Karstens, Abhijeet Mishra,
Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportYields(gdx)
} # }
```
