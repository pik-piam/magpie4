# reportSDG15

reports all SDG indicators relevant for SD15 - Life on Land

## Usage

``` r
reportSDG15(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

MAgPIE object

## SDG15 Life on land variables

|                                                                 |                     |                                                                |
|-----------------------------------------------------------------|---------------------|----------------------------------------------------------------|
| Name                                                            | Unit                | Meta                                                           |
| SDG\|SDG15\|Forest share                                        | share of total land | Share of land covered by forest (primary, secondary, forestry) |
| SDG\|SDG15\|Primary forest share                                | share of total land | Share of land covered by primary forest                        |
| SDG\|SDG15\|Afforestation                                       | million ha          | Area of afforestation (NDC and additional)                     |
| SDG\|SDG15\|Other natural land share                            | share of total land | Share of land covered by other natural land                    |
| SDG\|SDG15\|Terrestrial biodiversity                            | index               | Biodiversity Intactness Index (BII)                            |
| SDG\|SDG15\|Non-agricultural land share                         | share of total land | Share of land not used for agriculture                         |
| SDG\|SDG15\|Biological nitrogen fixation on cropland            | Mt N/yr             | Total biological nitrogen fixation on cropland                 |
| SDG\|SDG15\|Industrial and intentional biological fixation of N | Mt N/yr             | Sum of fertilizer and crop nitrogen fixation                   |

## Author

Benjamin Bodirsky, Isabelle Weindl

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportSDG15(gdx)
  } # }

```
