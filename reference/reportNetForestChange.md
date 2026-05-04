# reportNetForestChange

reports net and gross forest area change

## Usage

``` r
reportNetForestChange(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

NetForestChange as magclass object (Mha per year)

## Net forest change variables

|                            |        |                                        |
|----------------------------|--------|----------------------------------------|
| Name                       | Unit   | Meta                                   |
| Resources\|NetForestChange | Mha/yr | Annual net change in total forest area |

## Gross forest loss variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|GrossForestLoss | Mha/yr | Annual gross deforested area |
| Resources\|GrossForestLoss\|+\|Primary | Mha/yr | Annual gross loss of primary forest area |
| Resources\|GrossForestLoss\|+\|Secondary | Mha/yr | Annual gross loss of secondary forest area |
| Resources\|GrossForestLoss\|+\|Planted | Mha/yr | Annual gross loss of planted forest area |
| Resources\|GrossForestLoss\|Planted\|Plantation\|+\|Timber | Mha/yr | Annual gross loss of timber plantation area |
| Resources\|GrossForestLoss\|Planted\|Plantation\|+\|CO2-price AR | Mha/yr | Annual gross loss of CO2-price afforestation/reforestation plantation area |
| Resources\|GrossForestLoss\|Planted\|Natural\|+\|CO2-price AR | Mha/yr | Annual gross loss of CO2-price afforestation/reforestation natural forest area |
| Resources\|GrossForestLoss\|Planted\|Natural\|+\|NPI_NDC AR | Mha/yr | Annual gross loss of NPI/NDC afforestation/reforestation area |

## Gross forest gain variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|GrossForestGain | Mha/yr | Annual gross expansion of forest area |
| Resources\|GrossForestGain\|+\|Primary | Mha/yr | Annual gross expansion of primary forest area |
| Resources\|GrossForestGain\|+\|Secondary | Mha/yr | Annual gross expansion of secondary forest area |
| Resources\|GrossForestGain\|+\|Planted | Mha/yr | Annual gross expansion of planted forest area |
| Resources\|GrossForestGain\|Planted\|Plantation\|+\|Timber | Mha/yr | Annual gross expansion of timber plantation area |
| Resources\|GrossForestGain\|Planted\|Plantation\|+\|CO2-price AR | Mha/yr | Annual gross expansion of carbon plantation area through reforestation and/or afforestation with monoculture and/or non-native species |
| Resources\|GrossForestGain\|Planted\|Natural\|+\|CO2-price AR | Mha/yr | Annual gross reforestation and/or afforestation area for carbon sequestration with native tree species |
| Resources\|GrossForestGain\|Planted\|Natural\|+\|NPI_NDC AR | Mha/yr | Annual gross expansion of NPI/NDC afforestation/reforestation area |

## Author

Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportNetForestChange(gdx)
} # }
```
