# Kcal

Calculates the per-capita kcal consumption from the food demand model

## Usage

``` r
Kcal(
  gdx,
  file = NULL,
  level = "reg",
  products = "kfo",
  product_aggr = TRUE,
  after_shock = TRUE,
  calibrated = TRUE,
  magpie_input = FALSE,
  attributes = "kcal",
  per_capita = TRUE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "iso" ISO country codes, "reg"
  (regional), "glo" (global)

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr")

- product_aggr:

  aggregate over products or not (boolean)

- after_shock:

  FALSE is using the exogenous real income and the prices before a
  shock, TRUE is using the endogenous real income that takes into
  account food price change on real income

- calibrated:

  if FALSE, the true regression outputs are used, if TRUE the values
  calibrated to the start years are used

- magpie_input:

  TRUE or FALSE. This setting is only activate if arguments "calibrated"
  and "after_shock" are set to TRUE and else ignored. If set as TRUE,
  the per-capita kcal consumption values finally entering MAgPIE as
  input are used, which drive the behaviour of the MAgPIE model,
  excluding countries not listed in FAO. If set as FALSE, the per-capita
  kcal consumption values as calculated in the food demand model are
  used, including countries not listed in FAO.

- attributes:

  unit: kilocalories per day ("kcal"), g protein per day ("protein"). Mt
  reactive nitrogen ("nr").

- per_capita:

  per capita or aggregated for the population

## Value

calories as MAgPIE object (unit depends on per_capita: kcal/cap/day
(TRUE), kcal/day (FALSE))

## Details

Demand definitions are equivalent to FAO Food supply categories

## Author

Benjamin Leon Bodirsky, Isabelle Weindl

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- Kcal(gdx)
  } # }
```
