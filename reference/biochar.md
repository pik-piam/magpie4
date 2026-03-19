# biochar

Calculates biochar-related indicators from a MAgPIE gdx file. This
function provides all indicators for biochar-related material flows and
conversion, including biochar production, feedstock demand, and
long-term stable carbon storage in soils.

## Usage

``` r
biochar(
  gdx,
  indicator,
  level = "reg",
  feedstockAggr = FALSE,
  systemAggr = FALSE,
  attributes = "c",
  file = NULL
)
```

## Arguments

- gdx:

  GDX file

- indicator:

  Indicator types: bc_production, bc_feedstock_dem or bc_stable_carbon

- level:

  Spatial aggregation: "reg", "glo", or "regglo"

- feedstockAggr:

  If TRUE, aggregates over feedstock types. If not applicable for the
  selected indicator, set to FALSE (default).

- systemAggr:

  If TRUE, aggregates over bc_sys63 or biopyr_all63. If not applicable
  for the selected indicator, set to FALSE (default).

- attributes:

  Available output attributes: dry matter: Mt ("dm"), gross energy: PJ
  ("ge"), carbon: Mt C ("c"). Can also be a vector. The availability of
  attributes depends on the selected indicator.

- file:

  File name the output should be written to using write.magpie

## Value

Selected biochar-related indicator as MAgPIE object:

- `bc_production`: Biochar production (unit depends on `attributes`).
  Available attributes are "dm", "ge", and "c".

- `bc_feedstock_dem`: Biomass feedstock demand for biochar (unit depends
  on `attributes`). Available attributes are "dm", "ge", and "c".

- `bc_stable_carbon`: Stable C in soil from biochar after 100 years
  (mio. tC per yr). Available attribute is "c".

## Author

Isabelle Weindl

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- biochar(gdx, indicator = "bc_production", level = "regglo", feedstockAggr = TRUE)
} # }
```
