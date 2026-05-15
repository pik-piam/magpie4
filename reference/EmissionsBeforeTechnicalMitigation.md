# EmissionsBeforeTechnicalMitigation

reads GHG emissions before technical abatement out of a MAgPIE gdx file.
Technical abatement includes all abatement done in the MACC curves, but
exclude endogenous mitigation. These emissions are NOT the standard
reporting emissions, but used for special purposes like remind-magpie
coupling.

## Usage

``` r
EmissionsBeforeTechnicalMitigation(
  gdx,
  file = NULL,
  level = "reg",
  type = "co2_c",
  unit = "element",
  subcategories = FALSE
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

  emission type(s): "co2_c", "n2o_n" or "ch4" and in the case of
  unit="gas" "co2" and "n2o"

- unit:

  "element", "gas" or "co2eq"; "element": co2_c in Mt C/yr, n2o_n in Mt
  N/yr, ch4 in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr,
  ch4 in Mt CH4/yr; "co2eq": co2_c in Mt CO2/yr, n2o_n in Mt CO2eq/yr,
  ch4 in Mt CO2eq/yr

- subcategories:

  FALSE (default) or TRUE

## Value

emissions as MAgPIE object (unit depends on `unit`)

## Author

Florian Humpenoeder; Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- EmissionsBeforeTechnicalMitigation(gdx)
  } # }
```
