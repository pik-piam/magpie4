# reportGridLand

reports land-use from gridded (disaggregated) output

## Usage

``` r
reportGridLand(gdx)
```

## Arguments

- gdx:

  GDX file

## Value

land-use as MAgPIE object (million ha)

## Grid-level land use

This function produces grid-level (0.5 degree) land use data for land
cover categories (cropland, pasture, forest, urban, other land).
Variable names follow the reportingnames mapping.

## Author

Jannes Breier

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportGridLand(gdx)
  } # }

```
