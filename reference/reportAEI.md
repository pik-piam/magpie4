# reportAEI

reports Area equipped for Irrigation

## Usage

``` r
reportAEI(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Area equipped for Irrigation as MAgPIE object. Unit: see names

## Area equipped for irrigation variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Land Cover\|Cropland\|Area equipped for irrigation | million ha | Cropland area equipped with irrigation infrastructure |

## Author

Stephen Wirth

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportAEI(gdx)
  } # }
```
