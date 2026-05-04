# reportBioplasticDemand

reports demand for bioplastic and demand for substrate for bioplastic
production from MAgPIE results

## Usage

``` r
reportBioplasticDemand(gdx, detail = TRUE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  only relevant for substrate demand. If TRUE, substrate demand is
  disaggregated by crop type, if FALSE only the aggregated demand is
  reported.

- level:

  spatial aggregation to report bioplastic/substrate demand (only "reg"
  or "regglo")

## Value

bioplastic and bioplastic substrate demand as MAgPIE object

## Bioplastic demand variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Demand for bioplastic | Mt/yr | Total bioplastic demand |
| Demand for bioplastic substrate\|Total | Mt DM/yr | Total substrate demand for bioplastic production |

## Author

Debbora Leip

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportBioplasticDemand(gdx)
  } # }

```
