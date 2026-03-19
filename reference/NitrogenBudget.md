# NitrogenBudget

calculates projections of Nitrogen Budgets for Croplands (Tg Nr per)
from a MAgPIE gdx file

## Usage

``` r
NitrogenBudget(
  gdx,
  include_emissions = FALSE,
  level = "reg",
  debug = FALSE,
  cropTypes = FALSE,
  threshold = 0.05,
  progress = TRUE
)
```

## Arguments

- gdx:

  GDX file

- include_emissions:

  TRUE also divides the N surplus into different emissions

- level:

  aggregation level, reg, glo or regglo, cell, iso or grid

- debug:

  debug mode TRUE makes some consistency checks between estimates for
  different resolutions.

- cropTypes:

  FALSE for aggregate results; TRUE for crop-specific results

- threshold:

  passed to mstools::toolFertilizerDistribution

- progress:

  passed to mstools::toolFertilizerDistribution

## Author

Benjamin Leon Bodirsky, Michael Crawford, Edna J. Molina Bacca, Florian
Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- NitrogenBudget(gdx)
} # }
```
