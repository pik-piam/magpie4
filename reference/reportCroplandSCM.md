# reportCroplandSCM

Reports cropland area under soil carbon management (SCM)

## Usage

``` r
reportCroplandSCM(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level ("regglo" by default)

## Value

SCM area indicators as MAgPIE object (million ha)

## SCM area variables

|                                                                                           |            |                                                  |
|-------------------------------------------------------------------------------------------|------------|--------------------------------------------------|
| Name                                                                                      | Unit       | Meta                                             |
| Resources\|Land Cover\|Cropland\|Croparea under Soil Carbon Management                    | million ha | Total cropland area under soil carbon management |
| Resources\|Land Cover\|Cropland\|Croparea under Soil Carbon Management\|Share of Croparea | 1          | Share of cropland area under SCM                 |

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportCroplandSCM(gdx)
} # }
```
