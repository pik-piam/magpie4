# reportAAI

reports area actually irrigated

## Usage

``` r
reportAAI(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Area actually irrigated as MAgPIE object. Unit: see names

## Area actually irrigated variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Land Cover\|Cropland\|Area actually irrigated | million ha | Cropland area actually receiving irrigation |

## Author

Stephen Wirth, Anne Biewald

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportAEI(gdx)
} # }
```
