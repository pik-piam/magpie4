# productEmissions

Calculates GHG emissions by product, allocating emissions from different
sources (land-use change, enteric fermentation, manure management, etc.)
to specific agricultural products based on cropland shares, livestock
feed demand, and other factors. This is an improved version with better
code style and documentation.

## Usage

``` r
productEmissions(gdx, unit = "GWP100AR6", level = "reg", perTonne = TRUE)
```

## Arguments

- gdx:

  GDX file

- unit:

  "element", "gas", "GWP100AR5", "GWP100AR6", "GWP\*AR5", or "GWP\*AR6"
  "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4 in Mt CH4/yr "gas":
  co2_c in Mt CO2/yr, n2o_n in Mt N2O/yr, ch4 in Mt CH4/yr "GWP": co2_c
  in Mt CO2eq/yr, n2o_n in Mt CO2eq/yr, ch4 in Mt CO2eq/yr

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global)

- perTonne:

  If TRUE, returns emissions intensity (per tonne of production); if
  FALSE, returns total emissions

## Value

GHG emissions as MAgPIE object (Unit depends on `unit` parameter)

## See also

[`Emissions`](Emissions.md), [`emisCO2`](emisCO2.md)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- productEmissions(gdx, unit = "GWP100AR6", level = "reg")
  xIntensity <- productEmissions(gdx, unit = "GWP100AR6", perTonne = TRUE)
} # }
```
