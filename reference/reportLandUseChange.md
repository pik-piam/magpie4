# reportLandUseChange

reports land-use change

## Usage

``` r
reportLandUseChange(gdx, baseyear = 1995, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- baseyear:

  baseyear for calculating land-use change

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

land-use change as MAgPIE object (million ha wrt to baseyear)

## Land-use change variables

|                                                       |                         |                                                |
|-------------------------------------------------------|-------------------------|------------------------------------------------|
| Name                                                  | Unit                    | Meta                                           |
| Resources\|Land Cover Change\|Cropland                | million ha wrt baseyear | Change in cropland area relative to baseyear   |
| Resources\|Land Cover Change\|Pastures and Rangelands | million ha wrt baseyear | Change in pasture area relative to baseyear    |
| Resources\|Land Cover Change\|Forest                  | million ha wrt baseyear | Change in forest area relative to baseyear     |
| Resources\|Land Cover Change\|Other Land              | million ha wrt baseyear | Change in other land area relative to baseyear |

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportLandUseChange(gdx)
  } # }
```
