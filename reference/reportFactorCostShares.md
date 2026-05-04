# reportFactorCostShares

reports labor and capital cost share out of factor costs from MAgPIE
results

## Usage

``` r
reportFactorCostShares(gdx, type = "optimization", level = "regglo")
```

## Arguments

- gdx:

  GDX file

- type:

  - "requirements": shares from factor requirements

  - "optimization": cost shares between labor and capital costs in
    optimization

  - "accounting": cost shares based on accounting of labor and capital
    costs

- level:

  spatial aggregation: "reg", "glo", "regglo"

## Value

labor and capital cost shares as MAgPIE object

## Factor cost share variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Factor cost shares optimization\|Crop products\|+\|Labor cost share | % | Labor cost share in crop production |
| Factor cost shares optimization\|Crop products\|+\|Capital cost share | % | Capital cost share in crop production |
| Factor cost shares optimization\|Livestock products\|+\|Labor cost share | % | Labor cost share in livestock production |
| Factor cost shares optimization\|Livestock products\|+\|Capital cost share | % | Capital cost share in livestock production |

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportFactorCostShares(gdx)
} # }
```
