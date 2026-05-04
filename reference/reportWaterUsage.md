# reportWaterUsage

reports water usage for agricultural sector, crops and livestock and
non-agricultural sector

## Usage

``` r
reportWaterUsage(gdx, detail = TRUE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  logical. Setting to FALSE reports for agricultural sector, TRUE
  reports for combined, crops and livestock separately

- level:

  aggregation level of returned data ("regglo" by default)

## Value

water usage as MAgPIE object Unit: see names

## Agricultural water withdrawal variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Water\|Withdrawal\|Agriculture | km3/yr | Total agricultural water withdrawal |
| Resources\|Water\|Withdrawal\|Agriculture\|Crops | km3/yr | Water withdrawal for crop irrigation |
| Resources\|Water\|Withdrawal\|Agriculture\|Crops\|+\|Crops | km3/yr | Water withdrawal for food and feed crops |
| Resources\|Water\|Withdrawal\|Agriculture\|Crops\|+\|Bioenergy crops | km3/yr | Water withdrawal for bioenergy crops |
| Resources\|Water\|Withdrawal\|Agriculture\|Livestock\|+\|Livestock products | km3/yr | Water withdrawal for livestock |

## Agricultural water consumption variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Water\|Consumption\|Agriculture | km3/yr | Total agricultural water consumption |
| Resources\|Water\|Consumption\|Agriculture\|Crops | km3/yr | Water consumption for crop irrigation |
| Resources\|Water\|Consumption\|Agriculture\|Livestock | km3/yr | Water consumption for livestock |

## Non-agricultural water variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Water\|Withdrawal\|Non-agriculture | km3/yr | Total non-agricultural water withdrawal |
| Resources\|Water\|Withdrawal\|Non-agriculture\|+\|domestic | km3/yr | Water withdrawal for domestic use |
| Resources\|Water\|Withdrawal\|Non-agriculture\|+\|manufacturing | km3/yr | Water withdrawal for manufacturing sector |
| Resources\|Water\|Withdrawal\|Non-agriculture\|+\|electricity | km3/yr | Water withdrawal for electricity generation |
| Resources\|Water\|Consumption\|Non-agriculture | km3/yr | Total non-agricultural water consumption |

## Author

Florian Humpenoeder, Vartika Singh, Miodrag Stevanovic, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportWaterUsage(gdx)
} # }
```
