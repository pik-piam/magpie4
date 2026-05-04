# reportFertilizerNitrogen

Reports inorganic nitrogen application on crops

## Usage

``` r
reportFertilizerNitrogen(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  level of output

## Fertilizer nitrogen variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Nitrogen\|Inorganic Fertilizer Application | Mt Nr/yr | Total inorganic nitrogen fertilizer application |
| Resources\|Nitrogen\|Inorganic Fertilizer Application\|+\|Cereals | Mt Nr/yr | Nitrogen fertilizer application on cereals |
| Resources\|Nitrogen\|Inorganic Fertilizer Application\|+\|Oilcrops | Mt Nr/yr | Nitrogen fertilizer application on oilcrops |

## See also

[`NitrogenBudget`](NitrogenBudget.md)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportFertilizerNitrogen(gdx)
} # }
```
