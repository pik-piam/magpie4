# TimberProductionVolumetric

reads timber production out of a MAgPIE gdx file

## Usage

``` r
TimberProductionVolumetric(
  gdx,
  file = NULL,
  level = "regglo",
  sumProduct = FALSE,
  sumSource = TRUE
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

- sumProduct:

  sum over wood and woodfuel (TRUE/FALSE)

- sumSource:

  sum over timber sources: timber plantations, primary forest, secondary
  forest and non-forest land (woodfuel only) (TRUE/FALSE)

## Value

Timber production in mio. m3 per year

## Details

Annual timber production from timber plantations, primary forest,
secondary forest and non-forest land (woodfuel only). Converted from
mio. ton DM per year to mio. m3 per year using volumetric conversion
factors.

## Author

Abhijeet Mishra, Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- TimberProductionVolumetric(gdx)
} # }
```
