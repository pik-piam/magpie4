# reportCroparea

reports croparea

## Usage

``` r
reportCroparea(gdx, detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=FALSE, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Croparea as MAgPIE object (million ha/yr)

## Croparea variables

|                                                                  |            |                                                                                                 |
|------------------------------------------------------------------|------------|-------------------------------------------------------------------------------------------------|
| Name                                                             | Unit       | Meta                                                                                            |
| Resources\|Land Cover\|Cropland\|Croparea                        | million ha | Total physical cropland area used for crop production                                           |
| Resources\|Land Cover\|Cropland\|Croparea\|+\|Crops              | million ha | Cropland area for food and feed crops                                                           |
| Resources\|Land Cover\|Cropland\|Croparea\|Crops\|+\|Cereals     | million ha | Cropland for cereals (maize, rice, temperate cereals and tropical cereals)                      |
| Resources\|Land Cover\|Cropland\|Croparea\|Crops\|+\|Oil crops   | million ha | Cropland for oil crops (cotton seed, groundnuts, oilpalms, other oil crops, soybean, sunflower) |
| Resources\|Land Cover\|Cropland\|Croparea\|Crops\|+\|Sugar crops | million ha | Cropland for sugar crops (sugar beet, sugar cane)                                               |
| Resources\|Land Cover\|Cropland\|Croparea\|Crops\|+\|Other crops | million ha | Cropland for other crops (fruits, vegetables, nuts, potatoes, pulses, tropical roots)           |
| Resources\|Land Cover\|Cropland\|Croparea\|+\|Bioenergy crops    | million ha | Cropland for second-generation bioenergy crops (short rotation grasses, short rotation trees)   |
| Resources\|Land Cover\|Cropland\|Croparea\|++\|Irrigated         | million ha | Irrigated cropland (physical area)                                                              |
| Resources\|Land Cover\|Cropland\|Croparea\|++\|Rainfed           | million ha | Rainfed cropland (physical area)                                                                |

## Author

Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportCroparea(gdx)
} # }
```
