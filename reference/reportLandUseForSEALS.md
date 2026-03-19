# reportLandUseForSEALS

Writes MAgPIE land use projections to a specific NetCDF that can be read
by the Spatial Economic Allocation Landscape Simulator (SEALS) model for
generating high resolution land use maps.

## Usage

``` r
reportLandUseForSEALS(
  outputdir = ".",
  magCellLand = "cell.land_0.5_share.mz",
  outFile = NULL,
  selectyears = c(2020, 2030, 2050)
)
```

## Arguments

- outputdir:

  output directory which contains cellular magpie output

- magCellLand:

  Disaggregated land use (grid-cell land area share) as magclass object
  or file (.mz) from a MAgPIE run.

- outFile:

  a file name the output should be written to using
  [`ncdf4::nc_create`](https://rdrr.io/pkg/ncdf4/man/nc_create.html) and
  [`ncdf4::ncvar_put`](https://rdrr.io/pkg/ncdf4/man/ancvar_put.html)

- selectyears:

  Numeric vector of years to provide data for.

## Value

Proportions of different land use classes per grid sell in a NetCDF
format.

## SEALS land use output

This function produces NetCDF files (not MAgPIE reporting variables)
with land use proportions for crop, past, primforest, secdforest,
forestry, urban, and other land types.

## Author

Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportLandUseForSEALS(
  magCellLand = "cell.land_0.5_share.mz",
  outFile = "cell.land_0.5_SEALS.nc",
  selectyears = c(2020, 2030, 2050)
)
} # }
```
