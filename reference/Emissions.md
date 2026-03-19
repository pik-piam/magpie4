# Emissions

reads GHG emissions out of a MAgPIE gdx file

## Usage

``` r
Emissions(
  gdx,
  file = NULL,
  level = "reg",
  type = "co2_c",
  unit = "element",
  subcategories = TRUE,
  cumulative = FALSE,
  lowpass = NULL,
  inorg_fert_split = TRUE
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

- type:

  emission type(s): "co2_c", "n2o_n" or "ch4"

- unit:

  "element", "gas", "GWP100AR5", "GWP100AR6", "GWP\*AR5", or "GWP\*AR6"
  "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr "gas":
  co2_c in Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt CH4/yr "GWP": co2_c
  in Mt CO2/yr, n2o_n in Mt CO2eq/yr, ch4 in Mt CO2eq/yr

- subcategories:

  FALSE (default) or TRUE

- cumulative:

  Logical; Determines if emissions are reported annually (FALSE) or
  cumulative (TRUE). The starting point for cumulative emissions is
  y1995.

- lowpass:

  number of lowpass filter iterations

- inorg_fert_split:

  if TRUE then inorganic fertilizer emissions are further disaggregated
  into pasture- and cropland-related emissions. Both the aggregated
  ("inorg_fert") and disaggregated values ("inorg_fert_crop",
  "inorg_fert_past)" are reported

## Value

emissions as MAgPIE object (unit depends on `unit`)

## Author

Florian Humpenoeder, Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- Emissions(gdx)
  } # }
```
