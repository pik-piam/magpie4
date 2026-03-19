# reportSDG6

reports all SDG indicators relevant for SDG6 - Access to Water

## Usage

``` r
reportSDG6(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

MAgPIE object

## SDG6 Water variables

|                                          |         |                                                     |
|------------------------------------------|---------|-----------------------------------------------------|
| Name                                     | Unit    | Meta                                                |
| SDG\|SDG06\|Fertilizer use               | Mt N/yr | Nitrogen fertilizer application on cropland         |
| SDG\|SDG06\|Nitrogen surplus on cropland | Mt N/yr | Nitrogen surplus from cropland budget               |
| SDG\|SDG06\|Agricultural water use       | km3/yr  | Agricultural water withdrawal during growing period |

## Author

Felicitas Beier, Isabelle Weindl

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportSDG6(gdx)
  } # }

```
