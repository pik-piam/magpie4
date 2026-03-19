# tc

Calculates TC rates based on a MAgPIE gdx file

## Usage

``` r
tc(
  gdx,
  file = NULL,
  level = "reg",
  annual = TRUE,
  avrg = FALSE,
  baseyear = 1995,
  type = "crop"
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- annual:

  If TRUE, annual values are reported. If FALSE, the values for the
  whole timestep are reported. If FALSE, avrg has no effect

- avrg:

  If FALSE the annual tc rates of the current period are returned,
  otherwise the average annual tc rate for the period tbase to tn is
  returned. tbase defaults to the first timestep (see baseyear)

- baseyear:

  Determines the base year timestep for annual tc calculation. Average
  tc rates for later timesteps are calculated with respect to baseyear.
  No tc rates for timesteps before baseyear are returned)

- type:

  currently only 'crop'

## Value

A MAgPIE object containing tc rates. Annual ones if annual=TRUE, for the
whole timestep if annual=FALSE.

## Author

Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
x <- tc(gdx)
} # }
```
