# reportRuralDemandShares

reports rural demand and production shares based on local consumption

## Usage

``` r
reportRuralDemandShares(gdx, type = "tradOnly", level = "regglo")
```

## Arguments

- gdx:

  GDX file

- type:

  Type of ratio that should be calculated

  - `all`: How much rural & trad demand as a share of all demand is
    satisfied locally

  - `tradOnly`: How much rural & trad demand as a share of rural & trad
    demand is satisfied locally

  - `potential`: How much total gridded demand is potentially satisfied
    by gridded production

- level:

  spatial aggregation: "reg", "glo", "regglo"

## Value

share of food demand at disaggregated level coming from local production
as MAgPIE object

## Rural demand share variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Share of Rural Demand Satisfied by Rural Production\|Primary Crop and Livestock Products | 0 - 1 | Share of rural demand met by local production |
| Share of Total Demand Satisfied by Rural Production\|Primary Crop and Livestock Products | 0 - 1 | Share of total demand met by rural production |
| Share of Total Demand Potentially Satisfied by Local Production | 0 - 1 | Potential local production share |

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportruralDemandShares(gdx)
} # }
```
