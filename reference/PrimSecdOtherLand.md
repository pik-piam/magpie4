# PrimSecdOtherLand

Calculates share of primary and secondary non-forest vegetation for
different aggregation levels based on gridded magpie output and initial
shares of primary and secondary non-forest vegetation.

## Usage

``` r
PrimSecdOtherLand(
  x,
  ini_file,
  ini_year = "y1995",
  file = NULL,
  level = "grid",
  unit = "Mha"
)
```

## Arguments

- x:

  Time series of land pools (model output) containing only one
  aggregated class for other land. Can be a file or magclass object.

- ini_file:

  Initialisation file for primary and secondary other land (e.g. based
  on 1995 MAgPIE land-use initialisation values). Must have the same
  spatial resolution as `x`.

- ini_year:

  Reference year for estimating primary and secondary other land shares,
  must be included in `ini_file`.

- file:

  a file name the output should be written to using `write.magpie`

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate. The unit of output for the cases above is Mha. If
  level "grid" is specified the unit of output can be chosen between
  "share" and "Mha".

- unit:

  "Mha" or "share". Defines the unit of the gridded output, see also
  `level`.

## Value

`x` including land area for primary and secondary non-forested
vegetation in MAgPIE (other land) as MAgPIE object; either as unit of
area (Mha) or as fraction of total land per grid cell (share).

## Author

Patrick v. Jeetze, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
x <- "./cell.land_0.5.nc"
land <- PrimSecdOtherLand(x)

# direct use of disaggregation output
land <- PrimSecdOtherLand(land_hr)
} # }
```
