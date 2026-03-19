# Land Carbon Sink Adjustment Factors

Indirect human-induced emissions in the land use system

## Usage

``` r
landCarbonSink(
  gdx,
  file = NULL,
  level = "reg",
  cumulative = FALSE,
  baseyear = 1995,
  source = "Grassi"
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global).

- cumulative:

  Logical; Determines if emissions are reported annually (FALSE) or
  cumulative (TRUE). The starting point for cumulative emissions is
  y1995.

- baseyear:

  Baseyear used for cumulative emissions (default = 1995)

- source:

  Currently only "Grassi", which uses pre-calculated adjustment factors
  from Grassi et al 2021 (DOI 10.1038/s41558-021-01033-6). Can be
  extended in the future to also include "PIK", based on data from
  LPJmL.

## Value

Land Carbon Sink Adjustment Factors (Mt CO2 per year or cumulative)

## Details

Calculates global and regional Land Carbon Sink Adjustment Factors

## Author

Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- landCarbonSink(gdx)
} # }
```
