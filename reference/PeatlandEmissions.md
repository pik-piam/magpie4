# PeatlandEmissions

reads peatland GHG emissions out of a MAgPIE gdx file

## Usage

``` r
PeatlandEmissions(
  gdx,
  file = NULL,
  level = "cell",
  unit = "gas",
  cumulative = FALSE,
  baseyear = 1995,
  lowpass = 0,
  sum = TRUE,
  intact = FALSE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "reg" (regional), "glo"
  (global), "regglo" (regional and global) or any aggregation level
  defined in superAggregateX. In addition "climate" for the 3 climate
  regions tropical, temperate and boreal is available.

- unit:

  global warming potential (GWP100AR6) or gas (gas)

- cumulative:

  FALSE (default) or TRUE

- baseyear:

  Baseyear used for cumulative emissions (default = 1995)

- lowpass:

  number of lowpass filter iterations (default = 0)

- sum:

  sum over land types TRUE (default) or FALSE

- intact:

  report GHG emissions from intact peatlands FALSE (default) or TRUE

## Value

Peatland GHG emissions in Mt CO2eq (if unit="gwp") or Mt of the
respective gas (if unit="gas")

## Details

Peatland GHG emissions: CO2, DOC, CH4 and N2O

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- PeatlandArea(gdx)
  } # }
```
