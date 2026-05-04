# reportGridCroparea

reports Croparea from gridded (disaggregated) output

## Usage

``` r
reportGridCroparea(gdx)
```

## Arguments

- gdx:

  GDX file

## Value

area of cropland as MAgPIE object (million ha)

## Grid-level croparea by irrigation

This function produces grid-level (0.5 degree) croparea data by crop
type and irrigation system. Variable names follow the reportingnames
mapping (e.g., Cereals.Rainfed, Oilcrops.Irrigated).

## Author

Jannes Breier

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportGridCroparea(gdx)
  } # }

```
