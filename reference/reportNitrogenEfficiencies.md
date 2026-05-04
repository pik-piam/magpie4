# reportNitrogenEfficiencies

Reports different nitrogen use efficiency indicators

## Usage

``` r
reportNitrogenEfficiencies(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Nitrogen efficiency variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Nitrogen\|Cropland Budget\|Nitrogen Use Efficiency complete | Mt Nr/Mt Nr | Complete nitrogen use efficiency on cropland |
| Resources\|Nitrogen\|Cropland Budget\|Nitrogen Use Efficiency basic | Mt Nr/Mt Nr | Basic nitrogen use efficiency (harvest/inputs) |
| Resources\|Nitrogen\|Cropland Budget\|Soil Nitrogen Uptake Efficiency | Mt Nr/Mt Nr | Soil nitrogen uptake efficiency |
| Resources\|Nitrogen\|Pasture Budget\|Nitrogen Use Efficiency complete | Mt Nr/Mt Nr | Complete nitrogen use efficiency on pastures |

## See also

`reportNitrogenEfficiencies`

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportNitrogenEfficiencies(gdx)
} # }
```
