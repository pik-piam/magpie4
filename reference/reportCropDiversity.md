# reportCropDiversity

reports crop diversity

## Usage

``` r
reportCropDiversity(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Supports "reg", "glo",
  "regglo", "grid" or a custom region aggregation level.

## Value

Crop diversity as MAgPIE object

## Crop diversity variables

|                                                          |          |                                                                  |
|----------------------------------------------------------|----------|------------------------------------------------------------------|
| Name                                                     | Unit     | Meta                                                             |
| Biodiversity\|Shannon crop area diversity index          | unitless | Crop type diversity based on area shares (higher = more diverse) |
| Biodiversity\|Inverted Simpson crop area diversity index | unitless | Crop type diversity based on area shares (higher = more diverse) |

## Author

Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportCropDiversity(gdx)
} # }
```
