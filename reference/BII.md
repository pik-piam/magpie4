# Biodiversity intactness index

calculates the area weighted biodiversity intactness index (BII) out of
a MAgPIE gdx file

## Usage

``` r
BII(
  gdx,
  file = NULL,
  level = "glo",
  mode = "auto",
  landClass = "sum",
  spatialWeight = NULL,
  adjusted = FALSE,
  bii_coeff = NULL,
  side_layers = NULL
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  level of regional aggregation; "cell" (magpie cluster level), "reg"
  (regional), "glo" (global), "regglo" (regional and global), "iso"
  (country level), "grid" (0.5 degree grid cell level).

- mode:

  "auto" (default), "from_grid", "MAgPIE" or "postprocessing".

  - "MAgPIE" reports the BV based on values from the MAgPIE biodiversity
    module.

  - "postprocessing" calculates the BV based on land information from
    MAgPIE (for versions where biodiversity module was not available
    yet).

  - "auto" uses "MAgPIE" if available and falls back to "postprocessing"
    otherwise.

  - "from_grid" calculates BII values from BII output and returns
    aggregated values at the aggregation level specified.

- landClass:

  "all" returns average BII values for all land classes of ov_bv, "sum"
  returns the weighted BII over all land classes of ov44_bv_weighted.

- spatialWeight:

  Spatial weight for aggregating BII values. Only relevant if mode is
  "from_grid", adjusted is TRUE, or level is either "grid" or "iso".

- adjusted:

  if "TRUE", function returns adjusted BII values (results have been
  adjusted for primary and secondary other land).

- bii_coeff:

  file containing BII coefficients. Only needed for mode =
  "postprocessing". NULL tries to automatically detected the file.

- side_layers:

  file containing LUH2 side layers. NULL tries to automatically detected
  the file.

## Value

Biodiversity intactness index (unitless)

## Details

Calculates global, regional and cluster-level biodiversity intactness
index (BII)

## Author

Patrick v. Jeetze, Florian Humpenoeder, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- BII(gdx)
} # }
```
