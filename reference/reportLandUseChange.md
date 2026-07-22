# reportLandUseChange

reports land-use change

## Usage

``` r
reportLandUseChange(gdx, baseyear = 1995, level = "regglo", annual = FALSE)
```

## Arguments

- gdx:

  GDX file

- baseyear:

  baseyear for calculating land-use change. Ignored if annual is TRUE.

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

- annual:

  If FALSE (default), report cumulative land-use change relative to
  baseyear (million ha wrt baseyear) for all land types. If TRUE, report
  the average annual net change rate per time step (million ha/yr) for
  primary forest and natural planted forest. The first time step has no
  preceding time step and is set to 0.

## Value

land-use change as MAgPIE object (million ha wrt to baseyear, or million
ha/yr if annual is TRUE)

## Land-use change variables

|                                                       |                         |                                                |
|-------------------------------------------------------|-------------------------|------------------------------------------------|
| Name                                                  | Unit                    | Meta                                           |
| Resources\|Land Cover Change\|Cropland                | million ha wrt baseyear | Change in cropland area relative to baseyear   |
| Resources\|Land Cover Change\|Pastures and Rangelands | million ha wrt baseyear | Change in pasture area relative to baseyear    |
| Resources\|Land Cover Change\|Forest                  | million ha wrt baseyear | Change in forest area relative to baseyear     |
| Resources\|Land Cover Change\|Other Land              | million ha wrt baseyear | Change in other land area relative to baseyear |

## Annual land-use change variables (annual = TRUE)

|                                                                              |               |                                                                             |
|------------------------------------------------------------------------------|---------------|-----------------------------------------------------------------------------|
| Name                                                                         | Unit          | Meta                                                                        |
| Resources\|Land Cover Change\|Forest\|Natural Forest\|Primary Forest\|Annual | million ha/yr | Average annual net change in primary forest area over the time step         |
| Resources\|Land Cover Change\|Forest\|Planted Forest\|Natural\|Annual        | million ha/yr | Average annual net change in natural planted forest area over the time step |

## Author

Florian Humpenoeder, Miodrag Stevanović

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportLandUseChange(gdx)
  } # }
```
