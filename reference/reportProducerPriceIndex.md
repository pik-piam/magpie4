# reportProducerPriceIndex

reports producer price index

## Usage

``` r
reportProducerPriceIndex(gdx, prod_groups = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- prod_groups:

  whether to return only product groups

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Producer price index as MAgPIE object Unit: see names

## Producer price index variables

|                                                                     |                |                                                    |
|---------------------------------------------------------------------|----------------|----------------------------------------------------|
| Name                                                                | Unit           | Meta                                               |
| Prices\|Index2020\|Agriculture\|Producer\|Primary food products     | Index 2020=100 | Producer price index for primary food products     |
| Prices\|Index2020\|Agriculture\|Producer\|Crops                     | Index 2020=100 | Producer price index for crops                     |
| Prices\|Index2020\|Agriculture\|Producer\|Livestock products        | Index 2020=100 | Producer price index for livestock products        |
| Prices\|Index2020\|Agriculture\|Producer\|Bioenergy                 | Index 2020=100 | Producer price index for bioenergy crops           |
| Prices\|Index2020\|Agriculture\|Producer\|All agricultural products | Index 2020=100 | Producer price index for all agricultural products |

## Author

Isabelle Weindl, David M CHen

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportProducerPriceIndex(gdx)
  } # }

```
