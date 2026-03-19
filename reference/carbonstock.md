# carbonstock

reads carbon stocks out of a MAgPIE gdx file

## Usage

``` r
carbonstock(
  gdx,
  file = NULL,
  level = "cell",
  sum_cpool = TRUE,
  sum_land = TRUE,
  subcategories = NULL,
  stockType = "actual"
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "reg" (regional), "glo"
  (global), "regglo" (regional and global) or any secdforest aggregation
  level defined in superAggregate

- sum_cpool:

  sum over carbon pool dimension (default = TRUE)

- sum_land:

  sum over land type dimension (default = TRUE)

- subcategories:

  NULL or vector of strings. If NULL, no subcategories are returned.
  Meaningful options are "crop, "forestry" and "other"

- stockType:

  carbon stock type (default = "actual"). Options: "actual",
  "previousLandPattern" and "previousCarbonDensity".

## Value

carbon stocks in MtC

## Details

carbon pools consist of vegetation carbon (vegc), litter carbon (litc)
and soil carbon (soilc)

## Author

Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- carbonstock(gdx)
} # }
```
